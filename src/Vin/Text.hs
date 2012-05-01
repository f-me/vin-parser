-- | Module for textual fields
module Vin.Text (
    TextField, Text,
    TypeError(..),
    decodeString, encodeString, withString,
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

import Data.Char (toUpper, isSpace)

import Data.List (intercalate, reverse, dropWhile)
import qualified Data.Map as M

import Text.Email.Validate (isValid)

import Vin.Text.String
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

-- | String field, trimmed
string :: Error e => Field e ByteString ByteString
string = withString (p . p) <$> field where
    p = reverse . dropWhile isSpace

-- | Uppercased string
upperString :: Error e => Field e ByteString ByteString
upperString = withString (map toUpper) <$> string

-- | Field with integer, just verify value
int :: Error e => Field e ByteString ByteString
int = do
    s <- string
    let
        validInt = matchInt $ reads $ decodeString s
        matchInt :: [(Int, String)] -> Bool
        matchInt [(_, tl)] = all isSpace tl
        matchInt _ = False
    when (not validInt) $
        throwError $ strMsg $ "Unable to convert field " ++ decodeString s ++ " to int"
    return s

-- | Value from table
table :: Error e => M.Map ByteString ByteString -> Field e ByteString ByteString
table t = do
    s <- string
    case M.lookup s t of
        Nothing -> throwError $ strMsg $ "Can't find any matches in table for " ++ decodeString s
        Just v -> return v
    
-- | Table matching
(<<~) :: String -> [String] -> M.Map ByteString ByteString
result <<~ synonims = M.fromList $ zip rs s where
    s = map encodeString synonims
    rs = map encodeString $ repeat result

-- | Value is one of
oneOf :: Error e => [String] -> Field e ByteString ByteString
oneOf lst = do
    s <- string
    when (not (decodeString s `elem` lst)) $
        throwError $ strMsg $
            "Expecting value one of [" ++ intercalate ", " lst ++ "], but got " ++ decodeString s
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
    when (not (isValid $ decodeString s)) $ throwError $ strMsg $ "Invalid e-mail format: " ++ decodeString s
    return s
