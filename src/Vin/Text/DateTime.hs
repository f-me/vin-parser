module Vin.Text.DateTime (
    timeFormat,
    posixLocale, posix,
    dublin
    ) where

import Control.Applicative
import Control.Monad.Error

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Char (isSpace)
import Data.Time
import Data.Time.Clock.POSIX
import System.Locale

import Vin.Text.String
import Vin.Field

-- | Date-time with format and locale
timeFormat :: (ParseTime t, Error e) => TimeLocale -> String -> Field e ByteString t
timeFormat loc fmt = do
    s <- field
    let
        cantParse = "Unable to parse time " ++ decodeString s ++ " with format " ++ fmt
    maybe (throwError $ strMsg cantParse) return $ parseTime loc fmt (decodeString s)

-- | Posix time with format
posixLocale :: Error e => TimeLocale -> String -> Field e ByteString POSIXTime
posixLocale loc fmt = utcTimeToPOSIXSeconds <$> timeFormat loc fmt

-- | Posix time with format and default locale
posix :: Error e => String -> Field e ByteString POSIXTime
posix = posixLocale defaultTimeLocale

-- | Dublin Julian date used it xlsx.
dublin :: Error e => Field e ByteString POSIXTime
dublin = field >>= toDublin where
    toDublin str = case tryInteger (trim str) of
        Nothing -> throwError $ strMsg $ "Unable to parse dublin time: " ++ decodeString str
        Just d -> return $ toPOSIX d

    trim = snd . C8.span isSpace . fst . C8.spanEnd isSpace
    tryInteger s = do
        (v, tl) <- C8.readInteger s
        if C8.all isSpace tl
            then return v
            else Nothing
    
    toPOSIX :: Integer -> POSIXTime
    toPOSIX d = utcTimeToPOSIXSeconds utct where
        day = ModifiedJulianDay $ d - 2 + 2415020 - 2400000
        utct = UTCTime day 0
