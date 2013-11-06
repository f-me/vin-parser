module Vin.Utils
    ( DataRow
    , decodeICU
    )

where

import Data.ByteString (ByteString)

import qualified Data.Map as M

import Data.Text.ICU.Convert
import Data.Text.Encoding as T

type DataRow = M.Map ByteString ByteString

-- | Decode a row to UTF-8 using an ICU input converter.
decodeICU :: Converter -> DataRow -> DataRow
decodeICU c m = M.map enc m'
  where
    m' = M.mapKeys enc m
    enc =  T.encodeUtf8 . toUnicode c 
