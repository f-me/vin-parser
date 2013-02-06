-- | Module with common model fields
-- for model definition
module Vin.ModelField (
    ModelField(..),
    (~::), (~::?),
    (<::),
    (<:),
	(<:=),
    typed,
    program,
    make,
    notNull,
    verifyType,
    
    module Vin.Text,
    module Vin.Text.Specific
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad.Error ()
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.String ()

import Vin.Field (verify)
import Vin.Row
import Vin.Model
import Vin.Text
import Vin.Text.Specific

-- | Model field
data ModelField a = ModelField {
    modelFieldType :: FieldType a,
    connectToRow :: Text a -> ModelRow }

-- | Field by name
(~::) :: String -> FieldType a -> ModelField a
name ~:: tp = ModelField tp $ \act -> (encodeString name, showField tp <$> act)

-- | Optional field by name, and ignores NoColumn errors
(~::?) :: String -> FieldType a -> ModelField (Maybe a)
name ~::? tp = ModelField fType (second withNoColumns . connect) where
    (ModelField fType connect) = name ~:: optField tp

-- | Connect field with columns
(<::) :: ModelField a -> Text a -> ModelRow
f <:: act = connectToRow f act

-- | Connect field with one column
(<:) :: ModelField a -> String -> ModelRow
f <: name = f <:: (name `typed` modelFieldType f)

-- | Constant value
(<:=) :: ModelField (Maybe ByteString) -> String -> ModelRow
f <:= value = f <:: pure (Just $ encodeString value)

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
notNull = verifyType (not . C8.null) "Field can't be empty"

-- | Verify field
verifyType :: (a -> Bool) -> String -> FieldType a -> FieldType a
verifyType p s f = f { fieldReader = v } where
    v = verify p (const $ InvalidType s) $ fieldReader f
