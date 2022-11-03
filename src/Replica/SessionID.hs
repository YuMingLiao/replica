module Replica.SessionID
  ( SessionID
  , genSessionId
  , encodeSessionId
  , decodeSessionId
  ) where

import qualified Data.ByteString                as B
import qualified Data.Text                      as T
import           Crypto.Random                  (MonadRandom(getRandomBytes))
import           Text.Hex                       (encodeHex, decodeHex)
import           Control.Monad                  (guard)

newtype SessionID = SessionID B.ByteString
  deriving (Eq, Ord, Show)

sessionIdByteLength :: Int
sessionIdByteLength = 16

genSessionId :: MonadRandom m => m SessionID
genSessionId = SessionID <$> getRandomBytes sessionIdByteLength

-- | SessionID's text representation, safe to use as url path segment
encodeSessionId :: SessionID -> T.Text
encodeSessionId (SessionID bs) = encodeHex bs

decodeSessionId :: T.Text -> Maybe SessionID
decodeSessionId t = do
  bs <- decodeHex t
  guard $ B.length bs == sessionIdByteLength
  pure $ SessionID bs
