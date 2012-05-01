module Vin.Text.Specific (
    colors,
    carModels,
    carMakers
    ) where

import Control.Monad.Error

import Data.ByteString (ByteString)
import qualified Data.Map as M

import Vin.Field
import Vin.Text

-- | Car colors
colors :: Error e => Field e ByteString ByteString
colors = oneOf ["red", "green", "blue"]

(<<=~) :: String -> [String] -> M.Map ByteString ByteString
str <<=~ alts = str <<~ (str : alts)

-- | Car models
carModels :: Error e => Field e ByteString ByteString
carModels = table $ M.unions [
  -- VW truck
  "Caddy"      <<=~ ["Кэдди", "кедди", "Кедди"],
  "Amarok"     <<=~ [],
  "Crafter"    <<=~ ["Крафтер"],
  "Transporter"<<=~ ["T5", "Т5", "Транспортер"],
  -- VW motor
  "Tiguan"     <<=~ ["Тигуан", "тигуан"],
  "Polo"       <<=~ ["Поло"],
  "Touareg"    <<=~ ["Туарег", "Тouareg"],
  "Passat"     <<=~ ["Пассат", "пассат", "Passft"],
  "Jetta"      <<=~ ["Джетта"],
  "Golf"       <<=~ ["Гольф", "гольф", "Гольф+"],
  "Touran"     <<=~ ["Туран"],
  "Phaeton"    <<=~ ["Фаэтон", "фаэтон"],
  "Eos"        <<=~ ["Эос"],
  "Scirocco"   <<=~ ["Сирокко"],
  "Caravelle"  <<=~ ["Каравелла"],
  "Multivan"   <<=~ ["Мультивен"],
  "Sharan"     <<=~ ["Шаран"],
  -- Opel
  "Astra"      <<=~ [],
  "Zafira"     <<=~ [],
  "Corsa"      <<=~ [],
  "Insignia"   <<=~ [],
  "Combo"      <<=~ [],
  "Meriva"     <<=~ [],
  "Antara"     <<=~ [],
  "Vectra"     <<=~ [],
  -- Hummer
  "FOCUS"      <<=~ ["Фокус", "Focus"],
  "ESCAPE"     <<=~ [],
  "MONDEO"     <<=~ ["Мондео", "Mondeo"],
  "FIESTA"     <<=~ ["Fiesta"],
  "FUSION"     <<=~ ["Fusion"],
  "COUGAR"     <<=~ [],
  "KUGA"       <<=~ [],
  "GALAXY"     <<=~ [],
  "EXPLORER"   <<=~ [],
  "MAVERICK"   <<=~ [],
  "TRANSIT"    <<=~ ["Transit"]]

-- | Car makers
carMakers :: Error e => Field e ByteString ByteString
carMakers = string
