{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Replica.Types where

import           Control.Exception              (Exception)
import           Data.Aeson                     ((.:), (.=))
import qualified Data.Aeson                     as A
import qualified Data.Text                      as T
import qualified Replica.VDOM                   as V

data Message = 
    MsgEvent Event
  | MsgCallCallback CallCallback

data Event = 
  Event
    { evtType        :: T.Text
    , evtEvent       :: A.Value
    , evtPath        :: [Int]
    , evtClientFrame :: Int
    } 
  deriving Show

data CallCallback = CallCallback A.Value Int

instance A.FromJSON Message where
  parseJSON (A.Object o) = do
    t <- o .: "type"
    case (t :: T.Text) of
      "event" -> MsgEvent <$> (Event
        <$> o .: "eventType"
        <*> o .: "event"
        <*> o .: "path"
        <*> o .: "clientFrame")
      "call" -> MsgCallCallback <$> (CallCallback
        <$> o .: "arg"
        <*> o .: "id")
      _ -> fail "Expected \"type\" == \"event\" | \"call\""
  parseJSON _ = fail "Expected object"


data Update
  = ReplaceDOM V.HTML
  | UpdateDOM Int (Maybe Int) [V.Diff]

instance A.ToJSON Update where
  toJSON (ReplaceDOM dom) = A.object
    [ "type" .= V.t "replace"
    , "dom"  .= dom
    ]
  toJSON (UpdateDOM serverFrame clientFrame ddiff) = A.object
    [ "type" .= V.t "update"
    , "serverFrame" .= serverFrame
    , "clientFrame" .= clientFrame
    , "diff" .= ddiff
    ]

newtype Callback = Callback Int
  deriving (Eq, A.ToJSON, A.FromJSON)

-- | Context passed to the user's update function.
--
-- Use @call@ to run JS on the client.
--
-- To return a result from the client to the server,
-- first use @registerCallback@ to specify what's to be done with the result
-- and to get a 'Callback'.
--
-- Then pass that @Callback@ as the first argument to @call@.
-- Within the JS statement you also pass to call you'll have that argument
-- available as the variable @arg@, which you can use as follows:
--
-- > callCallback(arg, <data-to-pass-to-the-server>)
data Context = Context
  { registerCallback   :: forall a. A.FromJSON a => (a -> IO ()) -> IO Callback
  , unregisterCallback :: Callback -> IO ()
  , call               :: forall a. A.ToJSON a => a -> T.Text -> IO ()
  }

-- | Error/Exception

data SessionAttachingError
  = SessionDoesntExist
  | SessionAlreadyAttached
  deriving (Eq, Show)

instance Exception SessionAttachingError

data SessionEventError
  = IllformedData
  | InvalidEvent
  deriving Show

instance Exception SessionEventError
