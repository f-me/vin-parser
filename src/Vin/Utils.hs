module Vin.Utils (
    DataRow,
    encodeCP1251, decodeCP1251
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Codec.Text.IConv

import qualified Data.Map as M

type DataRow = M.Map ByteString ByteString

encodeCP1251 :: DataRow -> DataRow
encodeCP1251 m = M.map enc m'
  where
    m' = M.mapKeys enc m
    enc s = B.concat . L.toChunks . convert "UTF-8" "CP1251" $ L.fromChunks [s]

decodeCP1251 :: DataRow -> DataRow
decodeCP1251 m = M.map enc m'
  where
    m' = M.mapKeys enc m
    enc s = B.concat . L.toChunks . convert "CP1251" "UTF-8" $ L.fromChunks [s]
