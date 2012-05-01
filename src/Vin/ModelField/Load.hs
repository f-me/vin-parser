{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Vin.ModelField.Load (
    parseDefinitions,
    parseMappings
    ) where

import Text.Peggy (peggy, space, defaultDelimiter, ParseError)
import qualified Text.Peggy as Peggy

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
-- vin = VIN RUS
-- vin2 = VIN
-- carMaker = Brand
-- name = Name + Surname
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
    = identifier "=" columns { ($1, $2) }

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
