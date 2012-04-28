{-# LANGUAGE OverloadedStrings #-}

-- | Module with model definitions
module Vin.Models (
    ford,

    models
    ) where

import Vin.Model
import Vin.ModelField

ford :: Model
ford = model "ford" "FORD" [
    program "ford",
    make "FORD",
    arcModelCode "ARC_MODEL_CODE",
    ffdsId "FFDS_ID",
    vin "VIN_NUMBER",
    dealerCode "DEALER_CODE",
    dealerName "DEALER_NAME"]

models :: [Model]
models = [ford]
