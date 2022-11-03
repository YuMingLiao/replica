{-# LANGUAGE OverloadedStrings   #-}
module Replica.Log
  ( Log(..)
  , InfoLog(..)
  , ErrorLog(..)
  , rlog
  , format
  , encodeTime
  ) where

import qualified Colog.Core                     as Co
import qualified Chronos                        as Ch
import qualified Data.Text                      as T
import Replica.SessionID (SessionID, encodeSessionId)

data Log
  = DebugLog T.Text
  | InfoLog  InfoLog
  | ErrorLog  ErrorLog
  deriving Show

data InfoLog
  = SessionCreated SessionID
  | SessionAttached SessionID
  | SessionDetached SessionID
  | SessionTerminated SessionID T.Text
  | WSAccepted SessionID
  | WSClosedByNotFound SessionID
  | WSClosedByGoingAwayCode SessionID
  | WSConnectionClosed SessionID
  | HTTPPrerender SessionID
  deriving Show
data ErrorLog
  = WSInvalidWSPath T.Text
  | WSClosedByInternalError SessionID T.Text
  | WSClosedByUnexpectedCode SessionID T.Text
  deriving Show

severity :: Log -> Co.Severity
severity (InfoLog _)  = Co.Info
severity (DebugLog _) = Co.Debug
severity (ErrorLog _) = Co.Error

toText :: Log -> T.Text
toText (DebugLog t) = t
toText (InfoLog l)  = case l of
  SessionCreated sid           -> "[SID:" <> formatSid sid <> "] Session created"
  SessionAttached sid          -> "[SID:" <> formatSid sid <> "] Session attached"
  SessionDetached sid          -> "[SID:" <> formatSid sid <> "] Session detached"
  SessionTerminated sid reason -> "[SID:" <> formatSid sid <> "] Session terminated(" <> reason <> ")"
  WSAccepted sid               -> "[SID:" <> formatSid sid <> "] WS request accepted"
  WSClosedByNotFound sid       -> "[SID:" <> formatSid sid <> "] WS closed by session not found"
  WSClosedByGoingAwayCode sid  -> "[SID:" <> formatSid sid <> "] WS closed by going away(code=1006)"
  WSConnectionClosed sid       -> "[SID:" <> formatSid sid <> "] WS connection closed"
  HTTPPrerender sid            -> "[SID:" <> formatSid sid <> "] HTTP pre-rendered"
toText (ErrorLog l)  = case l of
  WSInvalidWSPath path              -> "Invalid websocket path: " <> path
  WSClosedByInternalError sid mes   -> "[SID:" <> formatSid sid <> "] WS closed by internal error: " <> mes
  WSClosedByUnexpectedCode sid code -> "[SID:" <> formatSid sid <> "] WS closed by unexpected code: " <> code

formatSid :: SessionID -> T.Text
formatSid sid = T.take 6 (encodeSessionId sid) <> ".."

format :: (Ch.Time, Log) -> T.Text
format (t, l) = encodeTime t <> " [" <> T.pack (show (severity l)) <> "] " <> toText l

-- | Thin-utility for co-log's logging
rlog :: Co.HasLog env msg m => env -> msg -> m ()
rlog env msg = Co.getLogAction env Co.<& msg

-- e.g. 2019-08-17T03:33:51.99+0900
encodeTime :: Ch.Time -> T.Text
encodeTime time =
  Ch.encode_YmdHMSz offsetFormat subsecondPrecision Ch.w3c offsetDatetime
  where
    jstOffset          = Ch.Offset (9 * 60)
    offsetDatetime     = Ch.timeToOffsetDatetime jstOffset time
    offsetFormat       = Ch.OffsetFormatColonOff
    subsecondPrecision = Ch.SubsecondPrecisionFixed 2
