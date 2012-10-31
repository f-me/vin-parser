module Vin.Utils (
    DataRow,
    encodeCP1251, decodeCP1251
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

import Data.Encoding (decodeStrictByteString, encodeStrictByteString)
import Data.Encoding.CP1251

import qualified Data.Map as M

type DataRow = M.Map ByteString ByteString

encodeCP1251 :: DataRow -> DataRow
encodeCP1251 m = M.map enc m'
  where
    m' = M.mapKeys enc m
    enc bs = encodeStrictByteString CP1251 $ B.unpack bs

decodeCP1251 :: DataRow -> DataRow
decodeCP1251 m = M.map enc m'
  where
    m' = M.mapKeys enc m
    enc s = B.pack $ decodeStrictByteString CP1251 s

