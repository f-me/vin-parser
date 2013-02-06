-- | Module with model definition and parse function
module Vin.Model (
    Model(..),
	ModelRow,
    model,
    parse
    ) where

import Control.Arrow (first)
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.Either (partitionEithers)

import Vin.Text
import Vin.Row

-- | One row of model
type ModelRow = (ByteString, Text ByteString)

-- | Model data
data Model = Model {
    modelProgram :: String,
    modelFields :: [ModelRow] }

-- | Model definition
model :: String -> [ModelRow] -> Model
model = Model

-- | Try to parse row
-- Returns list of errors and parsed values
parse 
    :: Model
    -> M.Map ByteString ByteString
    -> ([RowError ByteString TypeError], [(ByteString, ByteString)])
parse m d = first concat $ partitionEithers $ map parseField $ modelFields m where
    parseField :: ModelRow -> Either [RowError ByteString TypeError] (ByteString, ByteString)
    parseField (name, parser) = row d $ ((,) name) <$> parser
