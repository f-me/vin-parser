module Vin.Text.DateTime (
    timeFormat,
    timeWith,
    posixLocale, posix
    ) where

import Control.Monad.Error

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Locale

import Vin.Field

-- | Date-time with format and locale
timeFormat :: (ParseTime t, Error e) => TimeLocale -> String -> Field e ByteString t
timeFormat loc fmt = do
    s <- field
    let
        cantParse = "Unable to parse time " ++ C8.unpack s ++ " with format " ++ fmt
    maybe (throwError $ strMsg cantParse) return $ parseTime loc fmt (C8.unpack s)

-- | Date-time with format converted back to ByteString
timeWith
    :: (ParseTime t, Error e)
    => TimeLocale
    -> String
    -> (t -> Either String ByteString)
    -> Field e ByteString ByteString
timeWith loc fmt back = timeFormat loc fmt >>= either' where
    either' = either (throwError . strMsg) return . back

-- | Posix time with format
posixLocale :: Error e => TimeLocale -> String -> Field e ByteString ByteString
posixLocale loc fmt = timeWith loc fmt toBS where
    toBS :: UTCTime -> Either String ByteString
    toBS = Right . C8.pack . show' . floor . utcTimeToPOSIXSeconds where
        show' :: Integer -> String
        show' = show

-- | Posix time with format and default locale
posix :: Error e => String -> Field e ByteString ByteString
posix = posixLocale defaultTimeLocale
