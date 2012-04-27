module Vin.Types (
    TypeError(..),
    string, int,
    table, (<<~),
    alt
    ) where

import Control.Monad
import Control.Monad.Error

import Data.ByteString (ByteString)
import qualified  Data.ByteString.Char8 as C8

import qualified Data.Map as M

import Data.String

import Vin.Field

data TypeError = InvalidType String
    deriving (Eq, Ord, Read, Show)

instance Error TypeError where
    noMsg = InvalidType ""
    strMsg = InvalidType

-- | String field
string :: Error e => Field e  ByteString ByteString
string = field

-- | Field with integer, just verify value
int :: Error e => Field e ByteString ByteString
int = verify checkInt failInt string where
    checkInt = maybe False (C8.null . snd) . C8.readInt
    failInt s = strMsg $ "Unable to convert field " ++ C8.unpack s ++ " to type Int"

-- | Value from table
table :: Error e => M.Map ByteString ByteString -> Field e ByteString ByteString
table t = do
    s <- field
    case M.lookup s t of
        Nothing -> throwError $ strMsg $ "Can't find any matches in table for " ++ C8.unpack s
        Just v -> return v

-- | Table matching
(<<~) :: String -> [String] -> M.Map ByteString ByteString
result <<~ synonims = M.fromList $ zip s rs where
    s = map fromString synonims
    rs = map fromString $ repeat result

-- | Try alternatives
-- Returns result of first succeeded alternative, or fails
-- with message specified
alt :: Error e => e -> [Field e a v] -> Field e a v
alt msg lst = foldr mplus mzero lst `catchError` (\e -> throwError msg)
