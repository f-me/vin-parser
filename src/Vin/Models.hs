module Vin.Models (
    ford,

    models,

    module Vin.Text
    ) where

import Vin.Text

ford :: TextModel
ford = model "ford" "FORD" [
    ("model", "MODEL" ~:: string),
    ("arcModelCode", "ARC_MODEL_CODE" ~:: string)]

models :: [TextModel]
models = [ford]
