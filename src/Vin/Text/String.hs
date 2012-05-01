module Vin.Text.String (
    encodeString, decodeString, withString
    ) where

import Data.ByteString (ByteString)

import qualified Codec.Text.IConv as IConv
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

-- | Encode string to UTF8
encodeString :: String -> ByteString
encodeString = E.encodeUtf8 . T.pack

-- | Decode string from UTF8
decodeString :: ByteString -> String
decodeString = T.unpack . E.decodeUtf8

-- | Perform action on decoded string
withString :: (String -> String) -> ByteString -> ByteString
withString f = encodeString . f . decodeString
