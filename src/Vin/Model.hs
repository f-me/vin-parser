-- | Module with model definition and parse function
module Vin.Model (
    -- * Model description
    Model(..),
    model,
    parse
    ) where

import Control.Arrow ((***))
import Control.Applicative (pure)
import Data.Either (partitionEithers)
import Data.List (nub)
import qualified Data.Map as M
import Data.String (fromString)
import Data.Text (Text)

import Vin.Field

-- | Model description
data Model = Model {
    modelProgram :: String,
    modelMapping :: [(String, Field Text)] }

-- | Create @Model@
model :: String -> [(String, Field Text)] -> Model
model name maps = Model name $ (("program", text) <~ pure (fromString name)) : maps

-- | Parse one row, returns list of errors and parsed result
parse :: Model -> Row -> ([FieldError], Row)
parse m r = ((nub . concat) *** M.fromList) $ partitionEithers $ map parseField $ modelMapping m where
    parseField :: (String, Field Text) -> Either [FieldError] (Text, Text)
    parseField (name, parser) = do
        v <- evalField parser r
        case v of
            Nothing -> Left []
            Just v' -> return (fromString name, v')
