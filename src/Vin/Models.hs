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
    program                   "ford",
    make                      "FORD",
    ffdsId                    "FFDS_ID",
    companyCode               "DEALER_CODE",
    companyLATName            "DEALER_NAME",
    validFrom                 "VALID_FROM",
    validUntil                "VALID_TO",
    vin                       "VIN_NUMBER",
    plateNumber               "LICENCE_PLATE_NUM",
    --carMake               "carMake",
    carModel                  "MODEL",
    arcModelCode              "ARC_MODEL_CODE",
    sellDate                  "FIRST_REGISTRATION_DATE",
    fordVehicleType           "VEHICLE_TYPE",
    fordCountryFirstSold      "COUNTRY_FIRST_SOLD",
    programRegistrationDate   "CREATION_DATE",
    milageTO                  "MILEAGE"]
    where
        fordVehicleType = "fordVehicleType" ~:~ int
        fordCountryFirstSold = "fordCountryFirstSold" ~:~ string

fordPlus :: Model
fordPlus = model "fordPlus" "FORD" [
    program                   "fordPlus",
    make                      "FORD",
    companyCode               "UR",
    vin                       "VIN",
    undefined]
        
models :: [Model]
models = [ford, fordPlus]
