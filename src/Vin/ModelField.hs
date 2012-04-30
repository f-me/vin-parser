-- | Module with common model fields
-- for model definition
module Vin.ModelField (
    ModelField,
    modelFieldType, modelField,
    (~:~),
    program,
    make,
    arcModelCode, ffdsId, vin, dealerCode, dealerName, validFrom, validUntil,
    plateNumber, carMaker, carModel, sellDate, programRegistrationDate,
    milageTO, companyCode, companyLATName, lastTODate, color, modelYear, companyName,
    contractNo, contractDate, ownerCompany, ownerContact, ownerName,
    cardNumber, ownerAddress, ownerPhone, previousVin, manager,
    serviceInterval, subProgramName, ownerLATName, ownerEmail,
    
    module Vin.Text,
    module Vin.Text.Specific
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.String

import Vin.Model
import Vin.Text
import Vin.Text.Specific

type ModelField a = a -> (String, Text ByteString)

-- | Create specific model field
-- For example:
-- > name = modelField "name" string
-- then use it:
-- > name "SomeName"
modelFieldType :: String -> (a -> Text ByteString) -> ModelField a
modelFieldType name fieldType v = (name, fieldType v)

-- | Create simple field by name and type
modelField :: String -> TextField ByteString -> ModelField String
modelField name fieldType v = (name, v ~:: fieldType)

-- | modelField
(~:~) :: String -> TextField ByteString -> ModelField String
(~:~) = modelField

-- | Program name
program :: ModelField String
program = modelFieldType "program" (pure . fromString)

-- | Make
make :: ModelField String
make = modelFieldType "make" (pure . fromString)

-- | Arc model code
arcModelCode, ffdsId, vin, dealerCode, dealerName, validFrom, validUntil,
    plateNumber, carMaker, carModel, sellDate, programRegistrationDate,
    milageTO, companyCode, companyLATName, lastTODate, color, modelYear, companyName,
    contractNo, contractDate, ownerCompany, ownerContact, ownerName,
    cardNumber, ownerAddress, ownerPhone, previousVin, manager,
    serviceInterval, subProgramName, ownerLATName, ownerEmail
     :: ModelField String

arcModelCode             = "arcModelCode"                   ~:~ string
carMaker                 = "carMaker"                       ~:~ carMakers
carModel                 = "carModel"                       ~:~ carModels
cardNumber               = "cardNumber"                     ~:~ string
color                    = "color"                          ~:~ colors
companyCode              = "companyCode"                    ~:~ string
companyLATName           = "companyLATName"                 ~:~ string
companyName              = "companyName"                    ~:~ string
contractDate             = "contractDate"                   ~:~ time
contractNo               = "contractNo"                     ~:~ string
dealerCode               = "dealerCode"                     ~:~ string
dealerName               = "dealerName"                     ~:~ string
ffdsId                   = "ffdsId"                         ~:~ string
lastTODate               = "lastTODate"                     ~:~ time
manager                  = "manager"                        ~:~ string
milageTO                 = "milageTO"                       ~:~ string
modelYear                = "modelYear"                      ~:~ time
ownerAddress             = "ownerAddress"                   ~:~ string
ownerCompany             = "ownerCompany"                   ~:~ string
ownerContact             = "ownerContact"                   ~:~ string
ownerEmail               = "ownerEmail"                     ~:~ email
ownerLATName             = "ownerLATName"                   ~:~ string
ownerName                = "ownerName"                      ~:~ string
ownerPhone               = "ownerPhone"                     ~:~ phone
plateNumber              = "plateNumber"                    ~:~ string
previousVin              = "vin2"                           ~:~ upperString
programRegistrationDate  = "programRegistrationDate"        ~:~ time
sellDate                 = "sellDate"                       ~:~ time
serviceInterval          = "serviceInterval"                ~:~ int
subProgramName           = "subProgramName"                 ~:~ string
validFrom                = "validFrom"                      ~:~ time
validUntil               = "validUntil"                     ~:~ time
vin                      = "vin"                            ~:~ upperString
