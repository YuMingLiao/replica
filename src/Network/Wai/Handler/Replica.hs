{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.Wai.Handler.Replica (
    Config (..),
    app,
) where

import qualified Chronos as Ch
import qualified Colog.Core as Co
import Control.Applicative ((<|>))
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, atomically, check)
import Control.Exception (SomeException (SomeException), catch, evaluate, throwIO, try)
import Control.Monad (forever, void)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Void (Void, absurd)
import Network.HTTP.Media (matchAccept, (//))
import Network.HTTP.Types (hAccept, methodGet, methodHead, status200, status404)
import Network.Wai (Application, Middleware, pathInfo, requestHeaders, requestMethod, responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, requestPath)
import qualified Network.WebSockets as WS
import Network.WebSockets.Connection (Connection, ConnectionOptions, acceptRequest, forkPingThread, pendingRequest, receiveData, receiveDataMessage, rejectRequest, sendClose, sendCloseCode, sendTextData)

import Control.Monad.Trans.Resource (ResourceT)
import Replica.Application (Frame (frameNumber, frameVdom), Session (..), Context, Callback (..), Call (..))
import qualified Replica.Application as S
import Replica.Log (Log, rlog)
import qualified Replica.Log as L
import Replica.SessionID (SessionID)
import qualified Replica.SessionID as SID
import Replica.SessionManager (SessionManage)
import qualified Replica.SessionManager as SM
import Replica.Types (Event (..), SessionAttachingError (SessionAlreadyAttached, SessionDoesntExist), SessionEventError (IllformedData), Update (ReplaceDOM, UpdateDOM))
import qualified Replica.VDOM as V
import qualified Replica.VDOM.Render as R
import qualified Data.Map as M
import Data.IORef 
import Control.Concurrent.MVar
import Debug.Trace
import Control.Monad (unless)

data Config st = Config
    { cfgTitle :: T.Text
    , cfgHeader :: V.HTML
    , cfgWSConnectionOptions :: ConnectionOptions
    , cfgMiddleware :: Middleware
    , cfgLogAction :: Co.LogAction IO (Ch.Time, Log)
    , -- | Time limit for first connect
      cfgWSInitialConnectLimit :: Ch.Timespan
    , -- | limit for re-connecting span
      cfgWSReconnectionSpanLimit :: Ch.Timespan
    , cfgInitial :: ResourceT IO st
    , cfgStep :: Context -> st -> ResourceT IO (Maybe (V.HTML, st, IO ()))
    }

-- | Create replica application.
app :: Config st -> (Application -> IO a) -> IO a
app cfg@Config{..} cb = do
    sm <- SM.initialize smcfg
    cbs <- newIORef (0, M.empty)
    calls <- newIORef [] 
    conn <- newEmptyMVar
    let wapp = websocketApp sm
    let bapp = cfgMiddleware $ backendApp cfg sm cbs conn calls
    withWorker (SM.manageWorker sm) $ cb (websocketsOr cfgWSConnectionOptions wapp bapp)
  where
    smcfg =
        SM.Config
            { SM.cfgLogAction = Co.cmapM tagTime cfgLogAction
            , SM.cfgWSInitialConnectLimit = cfgWSInitialConnectLimit
            , SM.cfgWSReconnectionSpanLimit = cfgWSReconnectionSpanLimit
            }
    tagTime a = (,) <$> Ch.now <*> pure a

-- WS path prefix is needed if you want to use reverse proxy like nginx.
-- Currently it is fixed to "/ws/"
-- TODO: Make it configurable

encodeToWsPath :: SessionID -> T.Text
encodeToWsPath sid = "/ws/" <> SID.encodeSessionId sid

decodeFromWsPath :: T.Text -> Maybe SessionID
decodeFromWsPath wspath = SID.decodeSessionId (T.drop 4 wspath)

-- TODO: / だと Accept: text/html 使わないやつがいる？
--
-- 1) Some browser accepts */* for /favicon.ico which triggers
-- unintended pre-rendering. This guard is for those who forgot to
-- handle /favicon.ico requests.
--
-- 2) We only accept GET/HEAD requests which accepts text/html or
-- request path is "/". The later condition is for crollers which
-- might not properly set accept headers. Pre-renders run even with
-- HEAD requests but session is not started/stored and required
-- resources are released.
--
-- 3) Just use "/" as ws path because warp discards HEAD response body
-- anyway.
backendApp :: Config st -> SessionManage -> IORef (Int, M.Map Int (A.Value -> IO ())) -> MVar Connection -> IORef [Call] -> Application
backendApp Config{..} sm cbs conn calls req respond
    | pathIs "/favicon.ico" =
        respond $ responseLBS status404 [] "" -- (1)
    | isProperMethod && (isAcceptable || pathIs "/") = do
        v <- SM.preRender sm app' isHead -- (2)
        case v of
            SM.PRRNothing ->
                respond $ responseLBS status200 [] ""
            SM.PRROnlyPrerender body -> do
                -- TODO: Add proper logging for HEAD requests.
                let html = V.ssrHtml cfgTitle "/" cfgHeader body -- (3)
                respond $ responseLBS status200 [("content-type", "text/html")] (renderHTML html)
            SM.PRRSessionStarted sid body -> do
                rlog sm $ L.InfoLog $ L.HTTPPrerender sid
                let html = V.ssrHtml cfgTitle (encodeToWsPath sid) cfgHeader body
                respond $ responseLBS status200 [("content-type", "text/html")] (renderHTML html)
    | otherwise =
        respond $ responseLBS status404 [] ""
  where
    app' =
        S.Application
            { S.cfgInitial = cfgInitial
            , S.cfgStep = cfgStep ctx
            , S.cfgConn = conn
            , S.cfgCalls = calls
            , S.cfgCallbacks = cbs
            }
    ctx = S.Context
        { registerCallback = \cb -> atomicModifyIORef' cbs $ \(cbId, cbs') ->
            ( ( cbId + 1
              , flip (M.insert cbId) cbs' $ \arg -> case A.fromJSON arg of
                  A.Success arg' -> cb arg'
                  A.Error e -> traceIO $ "callCallback: couldn't decode " <> show arg <> ": " <> show e
              )
            , Callback cbId
            )
        , unregisterCallback = \(Callback cbId') -> atomicModifyIORef' cbs $ \(cbId, cbs') ->
            ((cbId, M.delete cbId' cbs'), ())
        , call = \arg js -> do
                   mConn <- trace "tryReadMVar" $tryReadMVar conn
                   case mConn of
                        Nothing -> atomicModifyIORef' calls $ \calls -> (calls ++ [Call (A.toJSON arg) js], ())
                        Just conn -> do
                          trace (show (Call (A.toJSON arg) js))  $ sendTextData conn $ A.encode $ Call (A.toJSON arg) js
        }


    isAcceptable = isJust $ do
        ac <- lookup hAccept (requestHeaders req)
        matchAccept ["text" // "html"] ac

    isGet = requestMethod req == methodGet
    isHead = requestMethod req == methodHead
    isProperMethod = isGet || isHead

    pathIs path = ("/" <> T.intercalate "/" (pathInfo req)) == path

    renderHTML html =
        BL.fromStrict
            . TE.encodeUtf8
            . TL.toStrict
            . TB.toLazyText
            $ R.renderHTML html

websocketApp :: SessionManage -> ServerApp
websocketApp sm pendingConn = do
    let wspath = TE.decodeUtf8 $ requestPath $ pendingRequest pendingConn
    case decodeFromWsPath wspath of
        Nothing -> do
            -- TODO: what happens to the client side?
            rlog sm $ L.ErrorLog $ L.WSInvalidWSPath wspath
            rejectRequest pendingConn "invalid ws path"
        Just sid -> do
            conn <- acceptRequest pendingConn
            forkPingThread conn 30
            rlog sm $ L.InfoLog $ L.WSAccepted sid
            r <- try $
                SM.withSession sm sid $ \ses ->
                    do
                        v <- attachSessionToWebsocket conn ses
                        case v of
                            Just (SomeException e) -> internalErrorClosure conn sid e -- Session terminated by exception
                            Nothing -> normalClosure conn -- Session terminated gracefully
                        `catch` handleWSConnectionException conn sid ses
                        `catch` handleSessionEventError conn sid ses
                        `catch` handleSomeException conn sid ses

            -- サーバ側を再起動した場合、基本みんな再接続を試そうとして
            -- SessionDoesntExist エラーが発生する。ブラウザ側では再ロー
            -- ドを勧めるべし。
            case r of
                Left (e :: SessionAttachingError) ->
                    case e of
                        SessionDoesntExist -> sessionNotFoundClosure conn <* rlog sm (L.InfoLog $ L.WSClosedByNotFound sid)
                        SessionAlreadyAttached -> internalErrorClosure conn sid e
                Right _ ->
                    pure ()
  where
    -- Websocket(https://github.com/jaspervdj/websockets/blob/0f7289b2b5426985046f1733413bb00012a27537/src/Network/WebSockets/Types.hs#L141)
    -- CloseRequest(1006): When the page was closed(atleast with firefox/chrome). Termiante Session.
    -- CloseRequest(???):  ??? Unexected Closure code
    -- ConnectionClosed: Most of the time. Connetion closed by TCP level unintentionaly. Leave contxt for re-connecting.
    handleWSConnectionException ::
        Connection ->
        SessionID ->
        Session ->
        WS.ConnectionException ->
        IO ()
    handleWSConnectionException conn sid ses e = case e of
        WS.CloseRequest code _
            | code == closeCodeGoingAway -> S.terminateSession ses <* rlog sm (L.InfoLog $ L.WSClosedByGoingAwayCode sid)
            | otherwise -> S.terminateSession ses <* rlog sm (L.ErrorLog $ L.WSClosedByUnexpectedCode sid (T.pack (show code)))
        WS.ConnectionClosed -> rlog sm (L.InfoLog $ L.WSConnectionClosed sid)
        WS.ParseException _ -> S.terminateSession ses *> internalErrorClosure conn sid e
        WS.UnicodeException _ -> S.terminateSession ses *> internalErrorClosure conn sid e

    -- Rare. Problem occuered while event displatching/pasring.
    handleSessionEventError :: Connection -> SessionID -> Session -> SessionEventError -> IO ()
    handleSessionEventError conn sid ses e = do
        S.terminateSession ses
        internalErrorClosure conn sid e

    -- Rare. ??? don't know what happened
    handleSomeException :: Connection -> SessionID -> Session -> SomeException -> IO ()
    handleSomeException conn sid ses e = do
        S.terminateSession ses
        internalErrorClosure conn sid e

    -- We probably shouldn't show what caused the internal
    -- error. It'll just confuse users. For debug purpose use log.
    internalErrorClosure conn sid e = do
        _ <- trySome $ sendCloseCode conn closeCodeInternalError ("" :: T.Text)
        recieveCloseCode conn
        rlog sm $ L.ErrorLog $ L.WSClosedByInternalError sid (T.pack (show e))

    -- TODO: Currentlly doesn't work due to issue https://github.com/jaspervdj/websockets/issues/182
    -- recieveData を非同期例外で止めると、その後 connection が生きているのに Stream は close されてしまい、
    -- sendClose しようとすると ConnectionClosed 例外が発生する。
    -- fixed: https://github.com/kamoii/websockets/tree/handle-async-exception
    normalClosure conn = do
        _ <- trySome $ sendClose conn ("done" :: T.Text)
        recieveCloseCode conn

    -- IE とは区別して扱いため。
    --
    --  * Connection closed and before re-connecting it was terminated
    --  * Sever restarted
    --  * Rare case: SessionID which has valid form but
    --
    sessionNotFoundClosure conn = do
        _ <- trySome $ sendCloseCode conn closeCodeSessionNotFound ("" :: T.Text)
        recieveCloseCode conn

    -- After sending client the close code, we need to recieve
    -- close packet from client. If we don't do this and
    -- immideatly closed the tcp connection, it'll be an abnormal
    -- closure from client pov.
    recieveCloseCode conn = do
        _ <- trySome $ forever $ receiveDataMessage conn
        pure ()

    trySome = try @SomeException

    closeCodeInternalError = 1011
    closeCodeGoingAway = 1001
    closeCodeSessionNotFound = 4000 -- app original code

{- | Attacehes session to webcoket connection

 This function will block until:

   * Connection/Protocol-wise exception thrown, or
   * Session ends gracefully, returning `Nothing`, or
   * Session ends by exception, returning `Just SomeException`

 Some notes:

   * Assumes this session is not attached to any other connection. (※1)
   * Connection/Protocol-wise exception(e.g. connection closed by client) will not stop the session.
   * At least one frame will always be sent immediately. Even in a case where session is already
     over/stopped by exception. In those case, it sends one frame and immiedeatly returns.
   * First frame will be sent as `ReplaceDOM`, following frame will be sent as `UpdateDOM`
   * In some rare case, when stepLoop is looping too fast, few frame might be get skipped,
     but its not much a problems since those frame would have been shown only for a moment. (※2)

 ※1
 Actually, there is no problem attaching more than one connection to a single session.
 We can do crazy things like 'read-only' attach and make a admin page where you can
 peek users realtime page.

 ※2
 This framework is probably not meant for showing smooth animation.
 We can actually mitigate this by preserving recent frames, not just the latest one.
 Or use `chan` to distribute frames.
-}
attachSessionToWebsocket :: Connection -> Session -> IO (Maybe SomeException)
attachSessionToWebsocket conn ses = do
  calls <- readIORef (sesCalls ses)
  trace (show calls) $ sequence (map (sendTextData conn . A.encode) calls) 
  
  isDone <- tryPutMVar (sesConn ses) conn
  unless isDone (void $ swapMVar (sesConn ses) conn)
  
  trace "start withWorker event frame" $ withWorker eventLoop frameLoop
  where
    frameLoop :: IO (Maybe SomeException)
    frameLoop = do
        v@(f, _) <- atomically $ S.currentFrame ses
        sendTextData conn $ A.encode $ ReplaceDOM (frameVdom f)
        frameLoop' v

    frameLoop' :: (Frame, STM (Maybe Event)) -> IO (Maybe SomeException)
    frameLoop' (prevFrame, prevStepedBy) = do
        e <- atomically $ Left <$> getNewerFrame <|> Right <$> S.waitTerminate ses
        case e of
            Left (v@(frame, _), stepedBy) -> do
                diff <- evaluate $ V.diff (frameVdom prevFrame) (frameVdom frame)
                let updateDom = UpdateDOM (frameNumber frame) (evtClientFrame <$> stepedBy) diff
                sendTextData conn $ A.encode updateDom
                frameLoop' v
            Right result ->
                pure $ either Just (const Nothing) result
      where
        getNewerFrame = do
            v@(f, _) <- S.currentFrame ses
            check $ frameNumber f > frameNumber prevFrame
            s <- prevStepedBy -- This should not block if we implement propertly. See `Session`'s documenation.
            pure (v, s)

    eventLoop :: IO Void
    eventLoop = forever $ do
        ev' <- A.decode <$> receiveData conn
        ev <- maybe (throwIO IllformedData) pure ev'
        traceIO (show ev)
        case ev of
             Event {} -> atomically $ S.feedEvent ses ev
             CallCallback arg cbId -> do 
               (_, cbs') <- readIORef (sesCallbacks ses)
               case M.lookup cbId cbs' of
                 Just cb -> cb arg
                 Nothing -> pure ()



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

