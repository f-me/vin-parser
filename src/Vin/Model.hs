module Vin.Model (
    Model(..),
    model,
    (~::),
    parse
    ) where

import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.String
import Data.Traversable (sequenceA)

import Vin.Text
import Vin.Row

data Model = Model {
    modelProgram :: String,
    modelMake :: String,
    modelFields :: [(String, Text ByteString)] }

-- | Model definition
model :: String -> String -> [(String, Text ByteString)] -> Model
model = Model

(~::) :: String -> TextField ByteString -> Text ByteString
s ~:: f = column (fromString s) f
infixr 2 ~::

-- | Try to parse row
-- Returns list of errors or list of parsed values
parse
    :: Model
    -> M.Map ByteString ByteString
    -> Either [RowError ByteString TypeError] [ByteString]
parse m d = row d (sequenceA (map snd (modelFields m)))
