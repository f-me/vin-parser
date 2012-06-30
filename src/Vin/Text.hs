-- | Module for textual fields
module Vin.Text (
    FieldType(..),
    TextField, Text,
    TypeError(..),
    decodeString, encodeString, withString,
    byteString, upperByteString, string, upperString, int, intByte,
    table, (<<~), oneOf, oneOfByte, oneOfNoCase, oneOfNoCaseByte, oneOfBy, oneOfByByte,
    alt, optional, withDefault,
    time, phone, email,

    module Vin.Text.DateTime
    ) where

import Control.Applicative
import Control.Monad.Error

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8

import Data.Char (toUpper, isSpace, toLower)

import Data.Function

import Data.List (intercalate, find)
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

-- | Trimmed uppercased ByteString
upperByteString :: FieldType ByteString
upperByteString = FieldType id $ (C8.map toUpper <$> fieldReader byteString)
    
-- | String field, trimmed
string :: FieldType String
string = FieldType encodeString ((p . p . decodeString) <$> field) where
    p = reverse . dropWhile isSpace

-- | Uppercased string
upperString :: FieldType String
upperString = FieldType encodeString $ (map toUpper <$> fieldReader string)

-- | Integer field (through bytestring)
intByte :: FieldType Int
intByte = FieldType (encodeString . show) $ do
    s <- fieldReader byteString
    let
        result = do
            (v, tl) <- C8.readInt s
            if C8.all isSpace tl
                then return v
                else Nothing
        onError = throwError $ strMsg $ "Unable to convert field " ++ decodeString s ++ " to int"
    maybe onError return result

-- | Integer field
int :: FieldType Int
int = FieldType (encodeString . show) $ do
    s <- fieldReader string
    let
        s' = filter (not . isSpace) s
        result = case reads s' of
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

-- | Value from table through ByteString
tableByte :: M.Map ByteString ByteString -> FieldType ByteString
tableByte t = FieldType id $ do
    s <- fieldReader byteString
    case M.lookup s t of
        Nothing -> throwError $ strMsg $ "Can't find valuw " ++ decodeString s ++ " in table"
        Just v -> return v

-- | Table matching
(<<~) :: String -> [String] -> M.Map ByteString ByteString
result <<~ synonims = M.fromList $ zip rs s where
    s = map encodeString synonims
    rs = map encodeString $ repeat result

-- | Value is one of
oneOf :: [String] -> FieldType ByteString
oneOf = oneOfBy (==)

-- | Value is one of
oneOfByte :: [String] -> FieldType ByteString
oneOfByte = oneOfByByte (==)

-- | Value is one of ignoring case
oneOfNoCase :: [String] -> FieldType ByteString
oneOfNoCase = oneOfBy ((==) `on` map toLower)

-- | Value is one of ignoring case
oneOfNoCaseByte :: [String] -> FieldType ByteString
oneOfNoCaseByte = oneOfByByte ((==) `on` C8.map toLower)

-- | Value is one of with comparer
oneOfBy :: (String -> String -> Bool) -> [String] -> FieldType ByteString
oneOfBy isEqual lst = FieldType id $ do
    s <- fieldReader string
    case find (isEqual s) lst of
        Nothing -> throwError $ strMsg
            $ "Expecting value one of [" ++ intercalate ", " lst ++ "], but got " ++ s
        Just v -> return $ encodeString v

-- | Value is one of with comparer by ByteString
oneOfByByte :: (ByteString -> ByteString -> Bool) -> [String] -> FieldType ByteString
oneOfByByte isEqual lst = FieldType id $ do
    s <- fieldReader byteString
    let
        isEqual' :: ByteString -> String -> Bool
        isEqual' l r = isEqual l (encodeString r)
    case find (isEqual' s) lst of
        Nothing -> throwError $ strMsg
            $ "Expecting value one of [" ++ intercalate ", " lst
                ++ "], but got " ++ decodeString s
        Just v -> return $ encodeString v

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
        alt ("Can't parse time: " ++ s) $ dublin : map posix [
            "%m/%d/%Y",
            "%m/%e/%Y",
            "%m/%e/%Y %k:%M",
            "%d.%m.%Y",
            "%e %b %Y",
            "%e-%b-%Y",
            "%d-%b-%y",
            "%m-%d-%y",
            "%b %Y",
            "%Y-%m-%dT%H:%M:%S",
            "%d.%m.%Y %H:%M:%S"]

-- | Phone number
phone :: FieldType ByteString
phone = byteString

-- | e-mail
email :: FieldType ByteString
email = FieldType id $ do
    s <- fieldReader byteString
    let ss = decodeString s
    when (not (null ss) && not (isValid ss)) $
        throwError $ strMsg $ "Invalid e-mail format: " ++ decodeString s
    return s
