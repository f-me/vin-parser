{-# LANGUAGE OverloadedStrings #-}

module Vin.Models.Cars (
    loadValue,
    getObject, getMember,
    Entry(..)
    ) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T


-- | Load JSON value from file
loadValue :: FilePath -> IO (Maybe Value)
loadValue f = fmap decode $ C8.readFile f

-- | Get member object
getObject :: Value -> [String] -> Maybe Value
getObject v [] = Just v
getObject (Object obj) (s:ss) = HM.lookup (T.pack s) obj >>= (`getObject` ss)
getObject _ _ = Nothing

-- | Get member value (parsed object)
getMember :: FromJSON a => Value -> [String] -> Maybe a
getMember v m = do
    v' <- getObject v m
    case fromJSON v' of
        Success x -> Just x
        Error _ -> Nothing

-- | Car label
data Entry = Entry {
    value :: T.Text,
    label :: T.Text }
        deriving (Eq, Ord, Read, Show)

instance FromJSON Entry where
    parseJSON (Object v) = Entry <$> (v .: "value") <*> (v .: "label")
    parseJSON _ = empty
