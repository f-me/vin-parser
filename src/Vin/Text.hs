-- | Module for textual fields
module Vin.Text (
    TextField, Text,
    TypeError(..),
    string, upperString, int,
    table, (<<~), oneOf,
    alt, optional, withDefault,
    time, phone, email,

    module Vin.Text.DateTime
    ) where

import Control.Applicative
import Control.Monad.Error

import Data.ByteString (ByteString)
import qualified  Data.ByteString.Char8 as C8

import Data.Char (toUpper)

import Data.List (intercalate)
import qualified Data.Map as M

import Data.String

import Text.Email.Validate (isValid)

import Vin.Text.DateTime
import Vin.Row
import Vin.Field hiding (alt)
import qualified Vin.Field as F (alt)

type TextField a = Field TypeError ByteString a

type Text a = Row ByteString TypeError ByteString a

data TypeError = InvalidType String
    deriving (Eq, Ord, Read, Show)

instance Error TypeError where
    noMsg = InvalidType ""
    strMsg = InvalidType

-- | String field
string :: Error e => Field e ByteString ByteString
string = field

-- | Uppercased string
upperString :: Error e => Field e ByteString ByteString
upperString = C8.map toUpper <$> string

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

-- | Value is one of
oneOf :: Error e => [String] -> Field e ByteString ByteString
oneOf lst = do
    s <- field
    when (not (C8.unpack s `elem` lst)) $
        throwError $ strMsg $ "Value must be one of: " ++ intercalate ", " lst
    return s

-- | Alternatives
alt :: Error e => String -> [Field e ByteString ByteString] -> Field e ByteString ByteString
alt msg = F.alt (strMsg msg)

-- | Time
time :: Error e => Field e ByteString ByteString
time = alt "Can't parse time" $ map posix [
    "%m/%d/%Y",
    "%m/%e/%Y",
    "%m/%e/%Y %k:%M",
    "%d.%m.%Y",
    "%e %b %Y",
    "%e-%b-%Y",
    "%d-%b-%y",
    "%m-%d-%y",
    "%b %Y"]

-- | Phone number
phone :: Error e => Field e ByteString ByteString
phone = string

-- | e-mail
email :: Error e => Field e ByteString ByteString
email = do
    s <- string
    when (not (isValid (C8.unpack s))) $ throwError $ strMsg "Invalid e-mail format"
    return s
