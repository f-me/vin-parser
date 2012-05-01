-- | Module for textual fields
module Vin.Text (
    FieldType(..),
    TextField, Text,
    TypeError(..),
    decodeString, encodeString, withString,
    byteString, string, upperString, int,
    table, (<<~), oneOf,
    alt, optional, withDefault,
    time, phone, email,

    module Vin.Text.DateTime
    ) where

import Control.Applicative
import Control.Monad.Error

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8

import Data.Char (toUpper, isSpace)

import Data.List (intercalate)
import qualified Data.Map as M

import Data.Time.Clock.POSIX

import Text.Email.Validate (isValid)

import Vin.Text.String
import Vin.Text.DateTime
import Vin.Row
import Vin.Field hiding (alt)
import qualified Vin.Field as F (alt)

data FieldType a = FieldType {
    showField :: a -> ByteString,
    fieldReader :: TextField a }

type TextField a = Field TypeError ByteString a

type Text a = Row ByteString TypeError ByteString a

data TypeError = InvalidType String
    deriving (Eq, Ord, Read, Show)

instance Error TypeError where
    noMsg = InvalidType ""
    strMsg = InvalidType

-- | Trimmed ByteString
byteString :: FieldType ByteString
byteString = FieldType id (trim <$> field) where
    trim = snd . C8.span isSpace . fst . C8.spanEnd isSpace

-- | String field, trimmed
string :: FieldType String
string = FieldType encodeString ((p . p . decodeString) <$> field) where
    p = reverse . dropWhile isSpace

-- | Uppercased string
upperString :: FieldType String
upperString = FieldType encodeString $ (map toUpper <$> fieldReader string)

-- | Integer field
int :: FieldType Int
int = FieldType (encodeString . show) $ do
    s <- fieldReader string
    let
        result = case reads s of
            [(v, tl)] -> if all isSpace tl
                then Just v
                else Nothing
            _ -> Nothing
        onError = throwError $ strMsg $ "Unable to convert field " ++ s ++ " to int"
    maybe onError return result

-- | Value from table
table :: M.Map ByteString ByteString -> FieldType ByteString
table t = FieldType id $ do
    s <- fieldReader string
    case M.lookup (encodeString s) t of
        Nothing -> throwError $ strMsg $ "Can't find value " ++ s ++ " in table"
        Just v -> return v

-- | Table matching
(<<~) :: String -> [String] -> M.Map ByteString ByteString
result <<~ synonims = M.fromList $ zip rs s where
    s = map encodeString synonims
    rs = map encodeString $ repeat result

-- | Value is one of
oneOf :: [String] -> FieldType String
oneOf lst = FieldType encodeString $ do
    s <- fieldReader string
    when (s `notElem` lst) $ throwError $ strMsg $
        "Expecting value one of [" ++ intercalate ", " lst ++ "], but got " ++ s
    return s

-- | Alternatives
alt :: Error e => String -> [Field e ByteString a] -> Field e ByteString a
alt msg = F.alt (strMsg msg)

-- | Time
time :: FieldType POSIXTime
time = FieldType showTime times where
    showTime = encodeString . show' . floor
    show' :: Integer -> String
    show' = show
    times = do
        s <- fieldReader string
        alt ("Can't parse time: " ++ s) $ map posix [
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
phone :: FieldType String
phone = string

-- | e-mail
email :: FieldType String
email = FieldType encodeString $ do
    s <- fieldReader string
    when (not $ isValid s) $
        throwError $ strMsg $ "Invalid e-mail format: " ++ s
    return s
