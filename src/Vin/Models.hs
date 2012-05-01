{-# LANGUAGE OverloadedStrings #-}

-- | Module with model definitions
module Vin.Models (
    models
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Traversable

import Vin.Model
import Vin.ModelField

model' :: String -> String -> [(String, Text ByteString)] -> Model
model' p m fs = model p m ([program p, make m] ++ fs)

wordsField :: [String] -> Text ByteString
wordsField ws = C8.unwords <$> (sequenceA $ map (~:: string) ws)

ford :: Model
ford = model' "ford" "FORD" [
    fddsId                    "FDDS_ID",
    companyCode               "DEALER_CODE",
    companyLATName            "DEALER_NAME",
    validFrom                 "VALID_FROM",
    validUntil                "VALID_TO",
    vin                       "VIN_NUMBER",
    plateNumber               "LICENCE_PLATE_NO",
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
fordPlus = model' "fordPlus" "FORD" [
    companyCode               "UR",
    vin                       "VIN",
	carModel                  "Модель",
	sellDate                  "Дата первой продажи",
	lastTODate                "Дата прохождения ТО",
	milageTO                  "Пробег на момент прохождения ТО"]

vwMotor :: Model
vwMotor = model' "vwMotor" "VW" [
	color                     "Цвет авт",
	modelYear                 "Модельный год",
	vin                       "VIN",
	dealerCode                "Код дилера получателя",
	companyName               "Дилер получатель",
	contractNo                "№ Дог продажи Клиенту",
	contractDate              "Дата договора продажи",
	sellDate                  "Дата передачи АМ Клиенту",
	ownerCompany              "Компания получатель",
	ownerContact              "Контактное лицо получателя",
	ownerName                 "Фактический получатель АМ"]

vwCommercial :: Model
vwCommercial = model' "vwCommercial" "VW" [
	sellDate                  "Дата продажи",
	validUntil                "Дата окончания карты",
	companyName               "Продавец",
	cardNumber                "№ карты",
	vin                       "VIN",
	modelYear                 "модельный год",
	plateNumber               "госномер",
	carModel                  "модель",
	ownerName'                ["имя", "фамилия", "отчество"],
	ownerAddress'             ["адрес частного лица или организации", "город", "индекс"],
	ownerPhone'               ["тел1", "тел2"],
	ownerCompany              "название организации"]
	where
		ownerName' = "ownerName" ~::~ wordsField
		ownerAddress' = "ownerAddress" ~::~ wordsField
		ownerPhone' = "ownerPhone" ~::~ wordsField

opel :: Model
opel = model' "opel" "OPEL" [
	carModel                   "Model",
	vin                        "VIN",
	carMaker                   "Brand",
	companyCode                "Retail Dealer",
	sellDate                   "Retail Date",
	previousVin                "Previous VIN (SKD)"]

hummer :: Model
hummer = model' "hummer" "HUMMER" [
	companyCode                "Retail Dealer",
	sellDate                   "Retail Date",
	carMaker                   "Brand",
	carModel                   "Model",
	vin                        "VIN RUS",
	previousVin                "VIN"]

chevroletNAO :: Model
chevroletNAO = model' "chevroletNAO" "CHEVROLET" [
	companyCode "Retail Dealer",
	sellDate "Retail Date",
	carMaker "Brand",
	carModel "Model",
	vin "VIN RUS",
	previousVin "VIN"]

chevroletKorea :: Model
chevroletKorea = model' "chevroletKorea" "CHEVROLET" [
	carModel "Model",
	carMaker "Brand",
	vin "VIN",
	companyCode "Retail Dealer",
	previousVin "Previous VIN (SKD)",
	sellDate "Retail Date"]

cadillac :: Model
cadillac = model' "cadillac" "CADILLAC" [
	companyCode "Retail Dealer",
	sellDate "Retail Date",
	carMaker "Brand",
	carModel "Model",
	vin "VIN RUS",
	previousVin "VIN"]

vwRuslan :: Model
vwRuslan = model' "vwRuslan" "VW" [
	cardNumber "№",
	manager "ФИО ответственного лица, внесшего данные в XLS файл",
	carModel "Модель Автомобиля VW",
	vin "VIN номер Автомобиля VW",
	serviceInterval "Межсервисный интервал",
	lastTODate "Дата прохождения ТО (Дата регистрации в программе)",
	milageTO "Величина пробега на момент регистрации в Программе",
	validUntil "Программа действует до (Дата)"]

chartis :: Model
chartis = model' "chartis" "CHARTIS" [
	cardNumber "№",
	manager "ФИО ответственного лица, внесшего данные в XLS файл",
	carMaker "Марка Автомобиля",
	carModel "Модель Автомобиля",
	vin "VIN номер Автомобиля",
	validFrom "Дата регистрации в программе",
	validUntil "Программа действует до (Дата)"]

vwAvilon :: Model
vwAvilon = model' "vwAvilon" "VW" [
	cardNumber "Подрядковый номер клубной карты",
	manager "ФИО ответственного лица, внесшего данные в XLS файл",
	carModel "Модель Автомобиля VW",
	vin "VIN номер Автомобиля VW",
	serviceInterval "Межсервисный интервал",
	validFrom "Дата регистрации в программе",
	milageTO "Величина пробега на момент регистрации в Программе",
	validUntil "Программа действует до (Дата)"]

atlantM :: Model
atlantM = model' "atlant" "ATLANT" [
	cardNumber "Номер карты Atlant-M Assistance",
	subProgramName "Тип программы",
	manager "ФИО ответственного лица, внесшего данные в XLS файл",
	carModel "Модель Автомобиля VW",
	vin "VIN номер Автомобиля VW",
	serviceInterval "Межсервисный интервал, км",
	validFrom "Дата регистрации в программе",
	milageTO "Величина пробега на момент регистрации в Программе, км",
	validUntil "Программа действует до (Дата)"]

autocraft :: Model
autocraft = model' "autocraft" "AUTOCRAFT" [
	cardNumber "Подрядковый номер клубной карты",
	manager "ФИО ответственного лица, внесшего данные в XLS файл",
	ownerName "ФИО Клиента",
	carModel "Модель Автомобиля BMW",
	vin "VIN номер Автомобиля BMW (последние 7 знаков)",
	validFrom "Дата регистрации в программе",
	validUntil "Программа действует до (Даты)",
	milageTO "Величина пробега на момент регистрации в Программе"]

b2c :: Model
b2c = model' "b2c" "B2C" [
	validFrom "Дата активации карты",
	validUntil "Дата окончания срока дейсвия карты",
	manager "ФИО сотрудника торговой точки",
	cardNumber "Номер карты",
	subProgramName "Тип карты",
	ownerName' ["Фамилия клиента", "Имя клиента", "Отчество клиента"],
	ownerLATName' ["Фамилия клиента (Лат)", "Имя клиента (Лат)"],
	ownerAddress' [
		"Адрес места жительства Индекс",
		"Адрес места жительства Город",
		"Адрес места жительства Улица",
		"Адрес места жительства Дом",
		"Адрес места жительства Квартира"],
	ownerPhone' ["Телефон клиента Мобильный", "Телефон клиента Домашний"],
	ownerEmail "Е-МAIL клиента",
	carMaker "Марка автомобиля",
	carModel "Модель автомобиля",
	modelYear "Год выпуска",
	plateNumber "Гос номер",
	vin "Идентификационный номер (VIN)",
	ownerContact' ["Фамилия доверенного лица", "Имя доверенного лица", "Отчество доверенного лица"]]
	where
		ownerName' = "ownerName" ~::~ wordsField
		ownerLATName' = "ownerLATName" ~::~ wordsField
		ownerAddress' = "ownerAddress" ~::~ wordsField
		ownerPhone' = "ownerPhone" ~::~ wordsField
		ownerContact' = "ownerContact" ~::~ wordsField

models :: [Model]
models = [ford, fordPlus, vwMotor, vwCommercial, opel, hummer, chevroletNAO,
	chevroletKorea, cadillac, vwRuslan, chartis, vwAvilon, atlantM, autocraft,
	b2c]
