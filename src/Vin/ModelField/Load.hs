module Vin.ModelField.Load (
    ) where

import Data.ByteString (ByteString)
import qualified Data.Map as M

import Vin.Text

types :: M.Map String (TextField ByteString)
types = M.fromList [
    ("color", color),
    ("date", time),
    ("int", int),
    ("string", string),
    ("time", time)]
