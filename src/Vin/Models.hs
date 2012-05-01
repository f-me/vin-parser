{-# LANGUAGE OverloadedStrings #-}

-- | Module with model definitions
module Vin.Models (
    models
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Char
import Data.Traversable

import Vin.Model
import Vin.ModelField

wordsF :: [String] -> Text String
wordsF ws = unwords <$> (sequenceA $ map (`typed` string) ws)

capitalized :: String -> Text ByteString
capitalized name = (capitalize . C8.uncons) <$> (name `typed` byteString) where
	capitalize Nothing = C8.empty
	capitalize (Just (h, t)) = C8.cons (toUpper h) t

model' :: String -> String -> [(String, Text ByteString)] -> Model
model' p m fs = model p m ([program p, make m] ++ fs)

ford :: Model
ford = model' "ford" "FORD" [
	-- fddsId <: "FDDS_ID",
    companyCode <: "DEALER_CODE",
    companyLATName <: "DEALER_NAME",
    validFrom <: "VALID_FROM",
    validUntil <: "VALID_TO",
    vin <: "VIN_NUMBER",
    plateNumber <: "LICENCE_PLATE_NO",
    --carMake               "carMake",
    carModel <: "MODEL",
    -- arcModelCode <: "ARC_MODEL_CODE",
    sellDate <: "FIRST_REGISTRATION_DATE",
    -- fordVehicleType <: "VEHICLE_TYPE",
    -- fordCountryFirstSold <: "COUNTRY_FIRST_SOLD",
    programRegistrationDate <: "CREATION_DATE",
    milageTO <: "MILEAGE"]

fordPlus :: Model
fordPlus = model' "fordPlus" "FORD" [
    companyCode <: "UR",
    vin <: "VIN",
	carModel <: "Модель",
	sellDate <: "Дата первой продажи",
	lastTODate <: "Дата прохождения ТО",
	milageTO <: "Пробег на момент прохождения ТО"]

vwMotor :: Model
vwMotor = model' "vwMotor" "VW" [
	carModel <: "Модель", -- TODO: Split this column
	color <: "Цвет авт",
	modelYear <: "Модельный год",
	vin <: "VIN",
	dealerCode <: "Код дилера получателя",
	companyName <: "Дилер получатель",
	contractNo <: "№ Дог продажи Клиенту",
	contractDate <: "Дата договора продажи",
	sellDate <: "Дата передачи АМ Клиенту",
	ownerCompany <: "Компания получатель",
	ownerContact <: "Контактное лицо получателя",
	ownerName <: "Фактический получатель АМ"]

vwCommercial :: Model
vwCommercial = model' "vwCommercial" "VW" [
	sellDate <: "Дата продажи",
	validUntil <: "Дата окончания карты",
	companyName <: "Продавец",
	cardNumber <: "№ карты",
	vin <: "VIN",
	modelYear <: "модельный год",
	plateNumber <: "госномер",
	carModel <:: ((head . C8.words) <$> ("модель" `typed` byteString)),
	ownerName <:: wordsF ["имя", "фамилия", "отчество"],
	ownerAddress <:: wordsF ["адрес частного лица или организации", "город", "индекс"],
	ownerPhone <:: wordsF ["тел1", "тел2"],
	ownerCompany <: "название организации"]

opel :: Model
opel = model' "opel" "OPEL" [
	carModel <: "Model",
	vin <: "VIN",
	carMaker <:: (("Brand" `typed` byteString) <|> pure (encodeString "Opel")),
	companyCode <: "Retail Dealer",
	sellDate <: "Retail Date",
	previousVin <: "Previous VIN (SKD)"]

hummer :: Model
hummer = model' "hummer" "HUMMER" [
	companyCode <: "Retail Dealer",
	sellDate <: "Retail Date",
	carMaker <: "Brand",
	carModel <: "Model",
	vin <: "VIN RUS",
	previousVin <: "VIN"]

chevroletNAO :: Model
chevroletNAO = model' "chevroletNAO" "CHEVROLET" [
	companyCode <: "Retail Dealer",
	sellDate <: "Retail Date",
	carMaker <: "Brand",
	carModel <: "Model",
	vin <: "VIN RUS",
	previousVin <: "VIN"]

chevroletKorea :: Model
chevroletKorea = model' "chevroletKorea" "CHEVROLET" [
	carModel <:: ((head . C8.words) <$> ("Model" `typed` byteString)),
	carMaker <:: (("Brand" `typed` carModels) <|> pure (encodeString "")),
	vin <: "VIN",
	companyCode <: "Retail Dealer",
	-- previousVin <: "Previous VIN (SKD)",
	sellDate <: "Retail Date"]

cadillac :: Model
cadillac = model' "cadillac" "CADILLAC" [
	companyCode <: "Retail Dealer",
	sellDate <: "Retail Date",
	carMaker <: "Brand",
	carModel <: "Model",
	vin <: "VIN RUS",
	previousVin <: "VIN"]

vwRuslan :: Model
vwRuslan = model' "vwRuslan" "VW" [
	cardNumber <: "№",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	carModel <:: (("Модель Автомобиля VW" `typed` byteString) <|> pure (encodeString "")),
	vin <: "VIN номер Автомобиля VW",
	serviceInterval <: "Межсервисный интервал",
	lastTODate <: "Дата прохождения ТО (Дата регистрации в программе)",
	milageTO <: "Величина пробега на момент регистрации в Программе",
	validUntil <: "Программа действует до (Дата)"]

chartis :: Model
chartis = model' "chartis" "CHARTIS" [
	cardNumber <: "№",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	carMaker <:: capitalized "Марка Автомобиля",
	carModel <:: capitalized "Модель Автомобиля",
	vin <: "VIN номер Автомобиля",
	validFrom <: "Дата регистрации в программе",
	validUntil <: "Программа действует до (Дата)"]

vwAvilon :: Model
vwAvilon = model' "vwAvilon" "VW" [
	carMaker <:: pure "VW",
	cardNumber <: "Подрядковый номер клубной карты",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	carModel <: "Модель Автомобиля VW",
	vin <: "VIN номер Автомобиля VW",
	serviceInterval <: "Межсервисный интервал",
	validFrom <: "Дата регистрации в программе",
	milageTO <: "Величина пробега на момент регистрации в Программе",
	validUntil <: "Программа действует до (Дата)"]

atlantM :: Model
atlantM = model' "atlant" "ATLANT" [
	carMaker <:: pure "VW",
	cardNumber <: "Номер карты Atlant-M Assistance",
	subProgramName <: "Тип программы",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	carModel <: "Модель Автомобиля VW",
	vin <: "VIN номер Автомобиля VW",
	serviceInterval <: "Межсервисный интервал, км",
	validFrom <: "Дата регистрации в программе",
	milageTO <: "Величина пробега на момент регистрации в Программе, км",
	validUntil <: "Программа действует до (Дата)"]

autocraft :: Model
autocraft = model' "autocraft" "AUTOCRAFT" [
	carMaker <:: pure "BMW",
	cardNumber <: "Подрядковый номер клубной карты",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	ownerName <: "ФИО Клиента",
	carModel <: "Модель Автомобиля BMW",
	vin <: "VIN номер Автомобиля BMW (последние 7 знаков)",
	validFrom <: "Дата регистрации в программе",
	validUntil <: "Программа действует до (Даты)",
	milageTO <: "Величина пробега на момент регистрации в Программе"]

b2c :: Model
b2c = model' "b2c" "B2C" [
	validFrom <: "Дата активации карты",
	validUntil <: "Дата окончания срока дейсвия карты",
	manager <: "ФИО сотрудника торговой точки",
	cardNumber <: "Номер карты",
	subProgramName <: "Тип карты",
	ownerName <:: wordsF ["Фамилия клиента", "Имя клиента", "Отчество клиента"],
	ownerLATName <:: wordsF ["Фамилия клиента (Лат)", "Имя клиента (Лат)"],
	ownerAddress <:: wordsF [
		"Адрес места жительства Индекс",
		"Адрес места жительства Город",
		"Адрес места жительства Улица",
		"Адрес места жительства Дом",
		"Адрес места жительства Квартира"],
	ownerPhone <:: wordsF ["Телефон клиента Мобильный", "Телефон клиента Домашний"],
	ownerEmail <: "Е-МAIL клиента",
	carMaker <:: capitalized "Марка автомобиля",
	carModel <: "Модель автомобиля",
	modelYear <: "Год выпуска",
	plateNumber <: "Гос номер",
	vin <: "Идентификационный номер (VIN)",
	ownerContact <:: wordsF ["Фамилия доверенного лица", "Имя доверенного лица", "Отчество доверенного лица"]]

models :: [Model]
models = [ford, fordPlus, vwMotor, vwCommercial, opel, hummer, chevroletNAO,
	chevroletKorea, cadillac, vwRuslan, chartis, vwAvilon, atlantM, autocraft,
	b2c]
