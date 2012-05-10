module Vin.Models.Cars (
    loadValue,
    getObject, getMember,
    Label(..)
    ) where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import System.FilePath

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
data Label = Label {
    label :: T.Text }
        deriving (Eq, Ord, Read, Show)

instance FromJSON Label where
    parseJSON (Object v) = Label <$> v .: (T.pack "label")
