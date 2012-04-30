module Vin.Text.Specific (
    colors,
    carModels,
    carMakers
    ) where

import Control.Monad.Error

import Data.ByteString (ByteString)
import qualified Data.Map as M

import Vin.Field
import Vin.Text

-- | Car colors
colors :: Error e => Field e ByteString ByteString
colors = oneOf ["red", "green", "blue"]

-- | Car models
carModels :: Error e => Field e ByteString ByteString
carModels = table $ M.unions [
    "FUSION" <<~ ["Fusion"],
    "FIESTA" <<~ ["Fiesta"]]

-- | Car makers
carMakers :: Error e => Field e ByteString ByteString
carMakers = table $ M.unions [
    "FORD" <<~ ["Ford"]]
