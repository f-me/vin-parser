-- | Module with common model fields
-- for model definition
module Vin.ModelField (
    ModelField,
    (~::),
    (<::),
    (<:),
    typed,
    program,
    make,
    notNull,
    arcModelCode, fddsId, vin, dealerCode, dealerName, validFrom, validUntil,
    plateNumber, carMaker, carModel, carMotor, carTransmission, sellDate, programRegistrationDate,
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

import Vin.Field (verify)
import Vin.Row
import Vin.Model
import Vin.Text
import Vin.Text.String
import Vin.Text.Specific

-- | Model field
data ModelField a = ModelField {
    modelFieldType :: FieldType a,
    connectToRow :: Text a -> (String, Text ByteString) }

-- | Field by name
(~::) :: String -> FieldType a -> ModelField a
name ~:: tp = ModelField tp $ \act -> (name, showField tp <$> act)

-- | Connect field with columns
(<::) :: ModelField a -> Text a -> (String, Text ByteString)
f <:: act = connectToRow f act

-- | Connect field with one column
(<:) :: ModelField a -> String -> (String, Text ByteString)
f <: name = f <:: (name `typed` (modelFieldType f))

-- | Make column action from column name and type
typed :: String -> FieldType a -> Text a
typed col tp = column (encodeString col) (fieldReader tp)

-- | Program name
program :: String -> (String, Text ByteString)
program p = ("program" ~:: string) <:: pure p

-- | Make
make :: String -> (String, Text ByteString)
make m = ("make" ~:: string) <:: pure m

-- | Not null string
notNull :: FieldType String -> FieldType String
notNull f = f { fieldReader = v } where
    v = verify (not . null) (const $ InvalidType "Field can't be empty") $ fieldReader f

arcModelCode             = "arcModelCode"                   ~:: string
carMaker                 = "carMake"                        ~:: carMakers
carModel                 = "carModel"                       ~:: carModels
carMotor                 = "motor"                          ~:: string
carTransmission          = "transmission"                   ~:: string
cardNumber               = "cardNumber"                     ~:: string
color                    = "color"                          ~:: string
companyCode              = "companyCode"                    ~:: string
companyLATName           = "companyLATName"                 ~:: string
companyName              = "companyName"                    ~:: string
contractDate             = "contractDate"                   ~:: time
contractNo               = "contractNo"                     ~:: string
dealerCode               = "dealerCode"                     ~:: string
dealerName               = "dealerName"                     ~:: string
fddsId                   = "fddsId"                         ~:: string
lastTODate               = "lastTODate"                     ~:: time
manager                  = "manager"                        ~:: string
milageTO                 = "milageTO"                       ~:: string
modelYear                = "modelYear"                      ~:: int
ownerAddress             = "ownerAddress"                   ~:: string
ownerCompany             = "ownerCompany"                   ~:: string
ownerContact             = "ownerContact"                   ~:: string
ownerEmail               = "ownerEmail"                     ~:: email
ownerLATName             = "ownerLATName"                   ~:: string
ownerName                = "ownerName"                      ~:: string
ownerPhone               = "ownerPhone"                     ~:: phone
plateNumber              = "plateNumber"                    ~:: string
previousVin              = "vin2"                           ~:: upperString
programRegistrationDate  = "programRegistrationDate"        ~:: time
sellDate                 = "sellDate"                       ~:: time
serviceInterval          = "serviceInterval"                ~:: int
subProgramName           = "subProgramName"                 ~:: string
validFrom                = "validFrom"                      ~:: time
validUntil               = "validUntil"                     ~:: time
vin                      = "vin"                            ~:: notNull upperString
