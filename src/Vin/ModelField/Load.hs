{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Vin.ModelField.Load (
    parseDefinitions,
    parseMappings,
    
    loadModel
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import Data.Traversable
    
import Text.Peggy (peggy, space, defaultDelimiter, ParseError)
import qualified Text.Peggy as Peggy

import Vin.Model
import Vin.Text
import Vin.ModelField

-- | Definition of field: name and type
type Definition = (String, String)

-- | Predefined fields
type Definitions = [Definition]

-- | Mapping from imported columns to internal field
type Mapping = (String, [String])

-- | Mappings
type Mappings = [Mapping]

-- | Grammar for definitions and mappings
-- Example of definitions file:
--
-- carModel : string
-- ownerEmail : string
-- sellDate : date
-- 
-- Example of mappings file:
--
-- vin <- VIN RUS
-- vin2 <- VIN
-- carMaker <- Brand
-- name <- Name + Surname
--
[peggy|

definitions :: Definitions
    = definition+ { $1 }

mappings :: Mappings
    = mapping+ { $1 }

definition :: Definition
    = identifier ":" fieldType { ($1, $2) }

identifier :: String
    = alpha

value :: String
    = anything

fieldType :: String
    = identifier

mapping :: Mapping
    = identifier "<-" columns { ($1, $2) }

columns :: [String]
    = column ("+" column)* { $1 : $2 }

column :: String
    = value

alpha ::: String
    = [a-zA-Z_0-9]+ { $1 }

anything ::: String
    = (!([=+\n\r;]) .)+ { $1 }

|]

parseDefinitions :: String -> Either ParseError Definitions
parseDefinitions = Peggy.parseString definitions "<input>"

parseMappings :: String -> Either ParseError Mappings
parseMappings = Peggy.parseString mappings "<input>"

loadModel :: String -> String -> String -> Either ParseError Model
loadModel name defSource mapSource = do
    defs <- parseDefinitions defSource
    maps <- parseMappings mapSource
    let
        defMap = M.fromList defs
        compileMapping :: Mapping -> (ByteString, Text ByteString)
        compileMapping (n, fs) = case M.lookup n defMap of
            Nothing -> error "No mapping"
            Just t -> (encodeString n, checkType t readfs)
            where
                readfs = C8.concat <$> sequenceA (map (`typed` byteString) fs)
    return $ Model name (program name : map compileMapping maps)

checkType :: String -> Text ByteString -> Text ByteString
checkType t f = maybe empty ($ f) $ M.lookup t fieldTypes where
    fieldTypes :: M.Map String (Text ByteString -> Text ByteString)
    fieldTypes = M.fromList [
        ("string", id)]
