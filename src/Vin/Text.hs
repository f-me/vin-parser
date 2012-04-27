module Vin.Text (
    TextValue, TextField, TextModel(..),
    model,
    (~::),
    parse,

    field,
    string, int, table, alt, (<<~),
    column,

    RowError(..), TypeError(..)
    ) where

import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.String
import Data.Traversable (sequenceA)

import Vin.Field
import Vin.Types
import Vin.Row

type TextValue = Field TypeError ByteString ByteString

type TextField = Row ByteString TypeError ByteString ByteString

data TextModel = TextModel {
    modelProgram :: String,
    modelMake :: String,
    modelFields :: [(String, TextField)] }

-- | Model definition
model :: String -> String -> [(String, TextField)] -> TextModel
model = TextModel

(~::) :: String -> TextValue -> TextField
s ~:: f = column (fromString s) f
infixr 2 ~::

-- | Try to parse row
-- Returns list of errors or list of parsed values
parse
    :: TextModel
    -> M.Map ByteString ByteString
    -> Either [RowError ByteString TypeError] [ByteString]
parse m d = row d (sequenceA (map snd (modelFields m)))
