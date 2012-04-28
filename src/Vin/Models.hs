{-# LANGUAGE OverloadedStrings #-}

module Vin.Models (
    ford,

    models
    ) where

import Control.Applicative (pure)

import Vin.Text
import Vin.Model

ford :: Model
ford = model "ford" "FORD" [
    ("program", pure "ford"),
    ("make", pure "FORD"),
    ("model", "MODEL" ~:: string),
    ("arcModelCode", "ARC_MODEL_CODE" ~:: string)]

models :: [Model]
models = [ford]
