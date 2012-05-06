-- | Module with model definition and parse function
module Vin.Model (
    Model(..),
	ModelRow,
    model,
    parse
    ) where

import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.Traversable (sequenceA)

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
-- Returns list of errors or list of parsed values
parse
    :: Model
    -> M.Map ByteString ByteString
    -> Either [RowError ByteString TypeError] [ByteString]
parse m d = row d (sequenceA (map snd (modelFields m)))

