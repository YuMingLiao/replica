{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Replica.Application (
    Application (..),
    Session (..),
    Frame (..),
    TerminatedReason (..),
    Context (..),
    Callback (..),
    Call (..),
    currentFrame,
    waitTerminate,
    feedEvent,
    terminateSession,
    isTerminated,
    terminatedReason,
    firstStep,
) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Async, async, cancel, pollSTM, race, waitCatchSTM)
import Control.Concurrent.STM (
    STM,
    TMVar,
    TQueue,
    TVar,
    atomically,
    isEmptyTMVar,
    newEmptyTMVar,
    newTMVar,
    newTQueue,
    newTVar,
    readTMVar,
    readTQueue,
    readTVar,
    retry,
    throwSTM,
    tryPutTMVar,
    writeTQueue,
    writeTVar,
 )
import Control.Exception (SomeException, evaluate, finally, mask, mask_, onException)
import Control.Monad (forever, join)
import Data.Bool (bool)
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Control.Concurrent.MVar (MVar)
import Network.WebSockets (Connection)
import Data.Maybe (isJust)
import Data.Void (Void, absurd)

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (ResourceT)
import qualified Control.Monad.Trans.Resource as RI
import Replica.Types (Event (..), SessionEventError (InvalidEvent))
import qualified Replica.VDOM as V
import qualified Replica.VDOM.Types as V hiding (t)
import Data.Aeson (FromJSON, ToJSON (..), Value, object, (.=))
import Data.Text (Text)
import Debug.Trace
import Data.Map
-- * Application

{- | Application

 * Blocking step could be resumed by
   1) internal change, like internal blocking io resumed, or by
   2) dispatched event
 * Event is dispatchable if `Event -> Maybe (IO ())' result is `Just fire'.
 * Event is dispatched only once per frame
S* In rare cases, event might dispatched after the `step' block is already resumed.
   In such case, dispatching shouldn't affect anything.

 * Manage resource(File Handlers, Threads, etc) using ResourceT/MonadResource.

* Network Lag problem:

Every HTML will be render to user with increasing unique id, called FrameID.
Each Event has FrameID attached to them which they occured(evClieentFrame).
Because of the network lag, we some times recieve Events generated from a past view.

Simple solution is to ignore such events and wait for a event generated by the
lastest view. But this will cause poor UI experience for users who has a slow
connection. Things like UI components not responding.

Instead we relax and receive events from past if its dispatchable. Thus the
event fireing function has type:

  Event -> Maybe (IO ())

`Nothing' means the event was undispatchable, meaning it was occured in a past
view, and also there was no corresponding elements in the latest view. `Just ev'
means the event occued by the lastest view, or it was from past, but there was a
correspnding element.

With this approuch, network lag problem is relaxed. But now we have to worry
about the follwing problem.

* Disptaching to a Wrong Element Problem:

TODO: WRITE
-}
data Application state = Application
    { cfgInitial :: {-Context ->-} ResourceT IO state
    , cfgStep :: state -> ResourceT IO (Maybe (V.HTML, state, IO ()))
    , cfgConn :: MVar Connection 
    , cfgCalls :: IORef [Call]
    , cfgCallbacks :: IORef (Int, Map Int (Value -> IO ()))
    }

-- Request header, Path, Query,
-- JS FFI
data Context = Context
    --jsCall :: forall a. FromJSON a => JSCode -> IO (Either JSError a)
    { registerCallback   :: forall a. FromJSON a => (a -> IO ()) -> IO Callback
    , unregisterCallback :: Callback -> IO ()
    , call               :: forall a. ToJSON a => a -> Text -> IO ()
    }

newtype Callback = Callback Int
  deriving (Eq, ToJSON, FromJSON)

data Call = Call Value Text deriving Show

instance ToJSON Call where
  toJSON (Call arg js) = object
    [ "type" .= V.t "call"
    , "arg"  .= arg
    , "js"   .= js
    ]



-- * Session

{- | Session is a running Application

  NOTES:

  * For every frame, its corresponding TMVar should get a value before the next (frame,stepedBy) is written.
    Only exception to this is when exception occurs, last setted frame's `stepedBy` could be empty forever.

 TODO: TMVar in a TVar. Is that a good idea?
 TODO: Note why we need TMVar (Maybe Event) for sesFrame.
-}
data Session = Session
    { sesFrame :: TVar (Frame, TMVar (Maybe Event))
    , sesEventQueue :: TQueue Event -- TBqueue might be better
    , sesThread :: Async ()
    , sesConn :: MVar Connection
    , sesCalls :: IORef [Call]
    , sesCallbacks :: IORef (Int, Map Int (Value -> IO ()))
    }

data Frame = Frame
    { frameNumber :: Int
    , frameVdom :: V.HTML
    , frameFire :: Event -> Maybe (IO ())
    }

data TerminatedReason
    = TerminatedGracefully
    | TerminatedByException SomeException

-- * Session operation

{- | Current frame.
There is always a frame even for terminated sessions.
-}
currentFrame :: Session -> STM (Frame, STM (Maybe Event))
currentFrame Session{sesFrame} = do
    (f, v) <- readTVar sesFrame
    pure (f, readTMVar v)

-- | Wait till session terminates.
waitTerminate :: Session -> STM (Either SomeException ())
waitTerminate Session{sesThread} =
    waitCatchSTM sesThread

-- | Feed Session an Event.
feedEvent :: Session -> Event -> STM ()
feedEvent Session{sesEventQueue} = writeTQueue sesEventQueue

{- | Kill Session

 * Do nothing if the session has already terminated.
 * Blocks until the session is actually terminated.
-}
terminateSession :: Session -> IO ()
terminateSession Session{sesThread} = cancel sesThread

{- | Check Session is terminated(gracefully or with exception)
Doesn't block.
-}
isTerminated :: Session -> STM Bool
isTerminated Session{sesThread} = isJust <$> pollSTM sesThread

terminatedReason :: Session -> STM (Maybe TerminatedReason)
terminatedReason Session{sesThread} = do
    e <- pollSTM sesThread
    pure $ either TerminatedByException (const TerminatedGracefully) <$> e
-- * Starting Application

{- | Execute the first step. Main purpose is to implement SSR.

Run the application till we get the first view. After we reach first view, the
application is suspended, though there might be threads running if application
invokes them before we reach first view.

(V.HTML, IO Session, IO ())

 * V.HTML      First view
 * IO Session  Continue the suspended appliction and manages it as Session.
 * IO ()       Release the resource acquired by application such as File Handlers/Threads..

Either `IO Session' or `IO ()' must be invoked orelase resrouce will
leak. Later(`IO ()') is for when we want descard this suspended application.

Some additional notes:

 * In rare case, application might not create any VDOM and gracefuly end. In
   such case, `Nothing` is returned.
 * If the application throws exception before we reach the fist view update,
   then the exception is simply raised.
 * Don't execute `firstStep' inside a mask.

Implementation notes:

 * リソース獲得及び解放ハンドラは mask された状態で実行される
 * 全体を onException で囲めないのは Nohting の場合は例外が発生していないが
   `releaseRes` を呼び出さないといけないため。
-}
firstStep :: Application state -> IO (Maybe (V.HTML, IO Session, IO ()))
firstStep Application{cfgInitial = initial, cfgStep = step, cfgConn = conn, cfgCalls = calls, cfgCallbacks = cbs} = mask $ \restore -> do
    doneVar <- newIORef False
    rstate <- RI.createInternalState
    let release = mkRelease doneVar rstate
    flip onException release $ do
        trace "In firstStep, onException" (pure ())
        r <- restore . flip RI.runInternalState rstate $ step =<< initial

        case r of
            Nothing -> do
                trace "r = Nothing" release
                pure Nothing
            Just (_vdom, state,  unblock) -> do
                vdom <- evaluate _vdom
                pure $
                    Just
                        ( trace "vdom evaluated" vdom
                        , trace "startSession passed" $ startSession conn calls cbs release step rstate (vdom, state, unblock)
                        , release
                        )
  where
    -- Make sure that `closeInternalState v` is called once.
    -- Do we need it??
    mkRelease doneVar rstate = mask_ $ do
        b <- atomicModifyIORef doneVar (True,)
        if b then pure () else RI.closeInternalState rstate

dispatchEvent :: V.HTML -> Event -> Maybe (IO ())
dispatchEvent html Event{..} =
    V.fireEvent html evtPath evtType (V.DOMEvent evtEvent)

startSession ::
    MVar Connection ->
    IORef [Call] ->
    IORef (Int, Map Int (Value -> IO ())) ->
    IO () ->
    (st -> ResourceT IO (Maybe (V.HTML, st, IO ()))) ->
    RI.InternalState ->
    (V.HTML, st, IO ()) ->
    IO Session
startSession conn calls cbs release step rstate (vdom, st, unblock) = flip onException release $ do
    let frame0 = Frame 0 vdom (const $ Just $ pure ())
    let frame1 = Frame 1 vdom (fmap (>> unblock) <$> dispatchEvent vdom)
    (fv, qv) <- atomically $ do
        r <- newTMVar Nothing
        f <- newTVar (frame0, r)
        q <- newTQueue
        pure (f, q)
    th <-
        async $
            withWorker
                (fireLoop (getNewFrame fv) (getEvent qv))
                (stepLoop (setNewFrame fv) step st frame1 `RI.runInternalState` rstate `finally` release)
    pure $ trace "Session passed" $ Session fv qv th conn calls cbs
  where
    setNewFrame var f = atomically $ do
        r <- newEmptyTMVar
        writeTVar var (f, r)
        pure r

    getNewFrame var = do
        v@(_, r) <- readTVar var
        bool retry (pure v) =<< isEmptyTMVar r

    getEvent que = readTQueue que
-- * Running Session

{- | stepLoop

 Every step starts with showing user the frame. After that we wait for a step to proceed.
 Step could be procceded by either:

   1) Client-side's event, which is recieved as `Event`, or
   2) Server-side event(e.g. io action returning a value)

 Every frame has corresponding `TMVar (Maybe Event)` called `stepedBy`. It is initally empty.
 It is filled whith `Event` when case (1), and filled with `Nothing` when case (2). (※1)
 New frame won't be created and setted before we fill current frame's `stepedBy`.

 ※1 Unfortunatlly, we don't have a garuntee that step was actually procceded by client-side event when
 `stepBy` is filled with `Just Event`. When we receive a dispatchable event, we fill `stepBy`
 before actually firing it. While firing the event, servier-side event could procceed the step.
-}
stepLoop ::
    (Frame -> IO (TMVar (Maybe Event))) ->
    (st -> ResourceT IO (Maybe (V.HTML, st, IO ()))) ->
    st ->
    Frame ->
    ResourceT IO ()
stepLoop setNewFrame step st frame = do
    stepedBy <- liftIO $ trace "stepLoop: setNewFrame" $ setNewFrame frame

    r <- trace "stepLoop: step st (stepWidget or Syn's cfgStep)" (step st) -- This should be the only blocking part
    liftIO $ traceIO "stepLoop: step outside (step st)" 
    _ <- liftIO . atomically $ trace "stepLoop: unblock" $ tryPutTMVar stepedBy Nothing
    case r of
        Nothing -> trace "stepLoop: step has no newSt" (pure ())
        Just (_newVdom, newSt, unblock) -> do
            newVdom <- liftIO $ evaluate _newVdom
            let newFrame = Frame (traceShow "stepLoop: step has newSt" (frameNumber frame + 1)) newVdom (fmap (>> unblock) <$> dispatchEvent newVdom)
            stepLoop setNewFrame step newSt newFrame

{- | fireLoop

 NOTE:
 Don't foregt that STM's (<|>) prefers left(no fairness like mvar).
 Because of (1), at (2) `stepedBy` could be already filled even though its in the same STM action.
-}
fireLoop ::
    STM (Frame, TMVar (Maybe Event)) ->
    STM Event ->
    IO Void
fireLoop getNewFrame getEvent = forever $ do
    (frame, stepedBy) <- atomically $ trace "fireLoop: getNewFrame" getNewFrame
    let act = atomically $ do
            r <- Left <$> getEvent <|> Right <$> readTMVar stepedBy -- (1)
            case r of
                Left ev -> case frameFire frame ev of
                    Nothing
                        | evtClientFrame ev < frameNumber frame ->
                            -- Event was undispatchable.
                            -- Event was from past frame, so we just ignore it wait for next event.
                            pure $ join act
                        | otherwise ->
                            -- Event was undipatchable even its was generated by current frame.
                            -- This means something is wrong(event is broken, dispatch logic has bug, etc).
                            throwSTM InvalidEvent
                    Just fire' -> do
                        -- Event is dispatcable.
                        -- Actually fire only if current frame's `step' is still blocking.
                        -- Though for rare case, just right after we tryPutTMVar,
                        -- the `step' blocking could resume before we actually fire event.
                        -- That means stepedBy is filled with a event that actually didn't resume `step'
                        stillBlocking <- tryPutTMVar stepedBy (Just ev) -- (2)
                        pure $ if stillBlocking then trace "fireLoop: fire'" fire' else trace "fireLoop: not blocking" (pure ())
                Right _ ->
                    pure $ trace "fireLoop: readTMVar. Seems unrelated to unblock" (pure ())
    join act

{- | Runs a worker action alongside the provided continuation.
 The worker will be automatically torn down when the continuation
 terminates.
-}
withWorker ::
    -- | Worker to run
    IO Void ->
    IO a ->
    IO a
withWorker worker cont =
    either absurd id <$> race worker cont
