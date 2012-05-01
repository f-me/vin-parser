module Vin.Text.DateTime (
    timeFormat,
    posixLocale, posix
    ) where

import Control.Applicative
import Control.Monad.Error

import Data.ByteString (ByteString)
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
