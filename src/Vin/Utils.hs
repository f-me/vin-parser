module Vin.Utils (
    DataRow,
    encodeCP1251, decodeCP1251
    ) where

import Data.ByteString (ByteString)

import Data.Encoding (decodeStrictByteString, encodeStrictByteString)
import Data.Encoding.CP1251
import Data.Encoding.UTF8

import qualified Data.Map as M

type DataRow = M.Map ByteString ByteString

encodeCP1251 :: DataRow -> DataRow
encodeCP1251 m = M.map enc m'
  where
    m' = M.mapKeys enc m
    enc = encodeStrictByteString CP1251 . decodeStrictByteString UTF8

decodeCP1251 :: DataRow -> DataRow
decodeCP1251 m = M.map enc m'
  where
    m' = M.mapKeys enc m
    enc =  encodeStrictByteString UTF8 . decodeStrictByteString CP1251

