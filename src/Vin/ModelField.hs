-- | Module with common model fields
-- for model definition
module Vin.ModelField (
    ModelField,
    (~::),
    (<::),
    (<:),
	(<:=),
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
import qualified Data.ByteString.Char8 as C8
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
    connectToRow :: Text a -> ModelRow }

-- | Field by name
(~::) :: String -> FieldType a -> ModelField a
name ~:: tp = ModelField tp $ \act -> (encodeString name, showField tp <$> act)

-- | Connect field with columns
(<::) :: ModelField a -> Text a -> ModelRow
f <:: act = connectToRow f act

-- | Connect field with one column
(<:) :: ModelField a -> String -> ModelRow
f <: name = f <:: (name `typed` (modelFieldType f))

-- | Constant value
(<:=) :: ModelField ByteString -> String -> ModelRow
f <:= value = f <:: pure (encodeString value)

-- | Make column action from column name and type
typed :: String -> FieldType a -> Text a
typed col tp = column (encodeString col) (fieldReader tp)

-- | Program name
program :: String -> ModelRow
program p = ("program" ~:: string) <:: pure p

-- | Make
make :: String -> ModelRow
make m = ("make" ~:: string) <:: pure m

-- | Not null string
notNull :: FieldType ByteString -> FieldType ByteString
notNull f = f { fieldReader = v } where
	v = verify (not . C8.null) (const $ InvalidType "Field can't be empty") $ fieldReader f

arcModelCode             = "modelCode"                      ~:: byteString
carMaker                 = "make"                           ~:: carMakers
carModel                 = "model"                          ~:: carModels
carMotor                 = "motor"                          ~:: byteString
carTransmission          = "transmission"                   ~:: byteString
cardNumber               = "cardNumber"                     ~:: byteString
color                    = "color"                          ~:: byteString
companyCode              = "companyCode"                    ~:: byteString
companyLATName           = "companyLATName"                 ~:: byteString
companyName              = "companyName"                    ~:: byteString
contractDate             = "contractDate"                   ~:: time
contractNo               = "contractNo"                     ~:: byteString
dealerCode               = "dealerCode"                     ~:: byteString
dealerName               = "dealerName"                     ~:: byteString
fddsId                   = "fddsId"                         ~:: byteString
lastTODate               = "checkupDate"                    ~:: time
manager                  = "manager"                        ~:: byteString
milageTO                 = "milageTO"                       ~:: byteString
modelYear                = "modelYear"                      ~:: int
ownerAddress             = "ownerAddress"                   ~:: byteString
ownerCompany             = "ownerCompany"                   ~:: byteString
ownerContact             = "ownerContact"                   ~:: byteString
ownerEmail               = "ownerEmail"                     ~:: email
ownerLATName             = "ownerLATName"                   ~:: byteString
ownerName                = "ownerName"                      ~:: byteString
ownerPhone               = "ownerPhone"                     ~:: phone
plateNumber              = "plateNum"                       ~:: byteString
previousVin              = "vin2"                           ~:: upperByteString
programRegistrationDate  = "programRegistrationDate"        ~:: time
sellDate                 = "buyDate"                        ~:: time
serviceInterval          = "serviceInterval"                ~:: int
subProgramName           = "subProgramName"                 ~:: byteString
validFrom                = "validFrom"                      ~:: time
validUntil               = "validUntil"                     ~:: time
vin                      = "vin"                            ~:: notNull upperByteString
