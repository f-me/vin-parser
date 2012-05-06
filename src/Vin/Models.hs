{-# LANGUAGE OverloadedStrings #-}

-- | Module with model definitions
module Vin.Models (
    models
    ) where

import Control.Applicative
import Control.Monad.Error
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Char
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.List (find, isPrefixOf, isSuffixOf)
import qualified Data.Map as M
import Data.Traversable

import Vin.Row (column)
import Vin.Model
import Vin.ModelField

wordsF :: [String] -> Text String
wordsF ws = unwords <$> (sequenceA $ map (`typed` string) ws)

capitalized :: String -> Text ByteString
capitalized name = (capitalize . C8.uncons) <$> (name `typed` byteString) where
	capitalize Nothing = C8.empty
	capitalize (Just (h, t)) = C8.cons (toUpper h) t

model' :: String -> [(String, Text ByteString)] -> Model
model' p fs = model p ([program p] ++ fs)

ford :: Model
ford = model' "ford" [
	carMaker <:: pure "Ford",
	-- fddsId <: "FDDS_ID",
    companyCode <: "DEALER_CODE",
    companyLATName <: "DEALER_NAME",
    validFrom <: "VALID_FROM",
    validUntil <: "VALID_TO",
    vin <: "VIN_NUMBER",
    plateNumber <: "LICENCE_PLATE_NO",
    --carMake               "carMake",
	fordModel <: "MODEL",
    -- arcModelCode <: "ARC_MODEL_CODE",
    sellDate <: "FIRST_REGISTRATION_DATE",
    -- fordVehicleType <: "VEHICLE_TYPE",
    -- fordCountryFirstSold <: "COUNTRY_FIRST_SOLD",
    programRegistrationDate <: "CREATION_DATE",
    milageTO <: "MILEAGE"]

fordPlus :: Model
fordPlus = model' "fordPlus" [
    companyCode <: "UR",
    vin <: "VIN",
	-- carMaker <: "carMake",
	fordModel <: "Модель",
	sellDate <: "Дата первой продажи",
	lastTODate <: "Дата прохождения ТО",
	milageTO <: "Пробег на момент прохождения ТО"]

data MotorModel = MotorModel {
	motorModelName :: String,
	motorMotor :: String,
	motorTransmission :: String }
		deriving (Eq, Ord, Read, Show)

vwMotor :: Model
vwMotor = model' "vwMotor" [
	carMaker <:: pure "VW",
	vwModel <:: (encodeString <$> column (encodeString "Модель") vwModelValue),
	-- vwModel <:: ((head . C8.words) <$> ("Модель" `typed` byteString)),
	-- TODO: Split this column and extract motor & transmission
	color <:: (column (encodeString "Цвет авт") vwColor),
	carMotor <:: (column (encodeString "Модель") vwMotorType),
	carTransmission <:: (column (encodeString "Модель") vwTransmission),
	modelYear <: "Модельный год",
	vin <: "VIN",
	dealerCode <: "Код дилера получателя",
	companyName <: "Дилер получатель",
	contractNo <: "No Дог продажи Клиенту",
	contractDate <: "Дата договора продажи",
	sellDate <: "Дата передачи АМ Клиенту",
	ownerCompany <: "Компания покупатель",
	ownerContact <: "Контактное лицо покупателя",
	ownerName <: "Фактический получатель ам"]
	where
		vwColor :: TextField String
		vwColor = do
			c <- fieldReader string
			let
				res = p . p $ c where
					p = reverse . snd . break (== '`')
			return $ if length res < 2 then "" else (init . tail $ res)

		isColor str
			| "`" `isPrefixOf` str && "`," `isSuffixOf` str = Just (init . init . tail $ str)
			| "`" `isPrefixOf` str && "`" `isSuffixOf` str = Just (init . tail $ str)
			| otherwise = Nothing

		vwModelValue :: TextField String
		vwModelValue = do
			c <- fieldReader string
			let failed = throwError $ strMsg $ "Invalid model value " ++ c
			maybe failed return . listToMaybe . words $ c

		vwMotorType :: TextField String
		vwMotorType = do
			c <- fieldReader string
			let
				failed = throwError $ strMsg $ "Can't extract motor " ++ c
				isMotor s = '.' `elem` s && length s == 3
			maybe failed return $ find isMotor (words c)

		vwTransmission :: TextField String
		vwTransmission = do
			c <- fieldReader string
			let
				failed = throwError $ strMsg $ "Can't extract transmission " ++ c
				fromTransmission s
					| "авт.-" `isPrefixOf` s = Just "auto"
					| "ручн.-" `isPrefixOf` s = Just "mech"
					| otherwise = Nothing
			maybe failed return $ listToMaybe $ mapMaybe fromTransmission $ words c

vwCommercial :: Model
vwCommercial = model' "vwCommercial" [
	carMaker <:: pure "VW",
	sellDate <: "Дата продажи",
	validUntil <: "Дата окончания карты",
	companyName <: "Продавец",
	cardNumber <: "№ карты",
	vin <: "VIN",
	modelYear <: "модельный год",
	plateNumber <: "госномер",
	vwModel <:: ((head . C8.words) <$> ("модель" `typed` byteString)),
	ownerName <:: wordsF ["имя", "фамилия", "отчество"],
	ownerAddress <:: wordsF ["адрес частного лица или организации", "город", "индекс"],
	ownerPhone <:: wordsF ["тел1", "тел2"],
	ownerCompany <: "название организации"]

opel :: Model
opel = model' "opel" [
	carMaker <:: (("carMake" `typed` byteString) <|> (pure "Opel")),
	opelModel <: "Model",
	vin <: "VIN",
	carMaker <:: (("Brand" `typed` byteString) <|> pure (encodeString "Opel")),
	companyCode <: "Retail Dealer",
	sellDate <: "Retail Date",
	previousVin <: "Previous VIN (SKD)"]

hummer :: Model
hummer = model' "hummer" [
	companyCode <: "Retail Dealer",
	sellDate <: "Retail Date",
	carMaker <:"Brand",
	hummerModel <:: (dropHummer <$> ("Model" `typed` byteString)),
	vin <: "VIN RUS",
	previousVin <: "VIN"]
	where
		dropHummer = C8.concat . take 1 . drop 1 . C8.words

chevroletNAO :: Model
chevroletNAO = model' "chevroletNAO" [
	companyCode <: "Retail Dealer",
	sellDate <: "Retail Date",
	carMaker <: "Brand",
	chevroletModel <: "Model",
	vin <: "VIN RUS",
	previousVin <: "VIN"]

chevroletKorea :: Model
chevroletKorea = model' "chevroletKorea" [
	chevroletModel <:: ((head . C8.words) <$> ("Model" `typed` byteString)),
	carMaker <:: (("Brand" `typed` carModels) <|> pure (encodeString "")),
	vin <: "VIN",
	companyCode <: "Retail Dealer",
	-- previousVin <: "Previous VIN (SKD)",
	sellDate <: "Retail Date"]

cadillac :: Model
cadillac = model' "cadillac" [
	companyCode <: "Retail Dealer",
	sellDate <: "Retail Date",
	carMaker <: "Brand",
	cadillacModel <:: (dropCadillac <$> ("Model" `typed` byteString)),
	vin <: "VIN RUS",
	previousVin <: "VIN"]
	where
		dropCadillac = C8.concat . take 1 . drop 1 . C8.words

vwRuslan :: Model
vwRuslan = model' "vwRuslan" [
	cardNumber <: "№",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	carMaker <:: pure "VW",
	vwModel <:: (rusVW <$> ("Модель Автомобиля VW" `typed` byteString)),
	vin <: "VIN номер Автомобиля VW",
	serviceInterval <: "Межсервисный интервал",
	lastTODate <: "Дата прохождения ТО (Дата регистрации в программе)",
	milageTO <: "Величина пробега на момент регистрации в Программе",
	validUntil <: "Программа действует до (Дата)"]

chartis :: Model
chartis = model' "chartis" [
	cardNumber <: "№",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	carMaker <:: capitalized "Марка Автомобиля",
	chartisModel <:: capitalized "Модель Автомобиля",
	vin <: "VIN номер Автомобиля",
	validFrom <: "Дата регистрации в программе",
	validUntil <: "Программа действует до (Дата)"]

vwAvilon :: Model
vwAvilon = model' "vwAvilon" [
	carMaker <:: pure "VW",
	cardNumber <: "Подрядковый номер клубной карты",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	vwModel <: "Модель Автомобиля VW",
	vin <: "VIN номер Автомобиля VW",
	serviceInterval <: "Межсервисный интервал",
	validFrom <: "Дата регистрации в программе",
	milageTO <: "Величина пробега на момент регистрации в Программе",
	validUntil <: "Программа действует до (Дата)"]

atlantM :: Model
atlantM = model' "atlant" [
	carMaker <:: pure "VW",
	cardNumber <: "Номер карты Atlant-M Assistance",
	subProgramName <: "Тип программы",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	vwModel <: "Модель Автомобиля VW",
	vin <: "VIN номер Автомобиля VW",
	serviceInterval <: "Межсервисный интервал, км",
	validFrom <: "Дата регистрации в программе",
	milageTO <: "Величина пробега на момент регистрации в Программе, км",
	validUntil <: "Программа действует до (Дата)"]

autocraft :: Model
autocraft = model' "autocraft" [
	carMaker <:: pure "BMW",
	cardNumber <: "Подрядковый номер клубной карты",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	ownerName <: "ФИО Клиента",
	bmwModel <: "Модель Автомобиля BMW",
	vin <: "VIN номер Автомобиля BMW (последние 7 знаков)",
	validFrom <: "Дата регистрации в программе",
	validUntil <: "Программа действует до (Даты)",
	milageTO <: "Величина пробега на момент регистрации в Программе"]

b2c :: Model
b2c = model' "b2c" [
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
	b2cModel <: "Модель автомобиля",
	modelYear <: "Год выпуска",
	plateNumber <: "Гос номер",
	vin <: "Идентификационный номер (VIN)",
	ownerContact <:: wordsF ["Фамилия доверенного лица", "Имя доверенного лица", "Отчество доверенного лица"]]

models :: [Model]
models = [ford, fordPlus, vwMotor, vwCommercial, opel, hummer, chevroletNAO,
	chevroletKorea, cadillac, vwRuslan, chartis, vwAvilon, atlantM, autocraft,
	b2c]

chevroletModel :: ModelField ByteString
opelModel :: ModelField ByteString
cadillacModel :: ModelField ByteString
vwModel :: ModelField ByteString
fordModel :: ModelField ByteString
bmwModel :: ModelField ByteString
hummerModel :: ModelField ByteString
b2cModel :: ModelField ByteString -- FIXME: Is this correct?
chartisModel :: ModelField ByteString -- FIXME: Is this correct?

-- Try map russian names of VW
rusVW :: ByteString -> ByteString
rusVW s = fromMaybe s $ M.lookup s rus where
	rus = M.unions [
		"Caddy"      <<~ ["Кэдди", "кедди", "Кедди"],
		"Crafter"    <<~ ["Крафтер"],
		"Transporter"<<~ ["T5", "Т5", "Транспортер"],
		"Tiguan"     <<~ ["Тигуан", "тигуан"],
		"Polo"       <<~ ["Поло"],
		"Touareg"    <<~ ["Туарег", "Тouareg"],
		"Passat"     <<~ ["Пассат", "пассат", "Passft"],
		"Jetta"      <<~ ["Джетта"],
		"Golf"       <<~ ["Гольф", "гольф", "Гольф+"],
		"Touran"     <<~ ["Туран"],
		"Phaeton"    <<~ ["Фаэтон", "фаэтон"],
		"Eos"        <<~ ["Эос"],
		"Scirocco"   <<~ ["Сирокко"],
		"Caravelle"  <<~ ["Каравелла"],
		"Multivan"   <<~ ["Мультивен"],
		"Sharan"     <<~ ["Шаран"]]

chevroletModel = "carModel" ~:: oneOfNoCase [
	"Alero",
	"Astra",
	"Aveo",
	"Beretta",
	"Blazer",
	"Camaro",
	"Caprice",
	"Captiva",
	"Cavalier",
	"Celta",
	"Cheyenne",
	"Cobalt",
	"Colorado",
	"Corsa",
	"Corsa Wind",
	"Corsica",
	"Corvette",
	"Epica",
	"Evanda",
	"Express",
	"HHR",
	"Ipanema GL",
	"Jimmy",
	"Kadett",
	"Lacetti",
	"lmpala",
	"Lumina",
	"Malibu",
	"Matiz",
	"Metro",
	"Monte Carlo",
	"Monza",
	"NIVA",
	"Omega",
	"Prism",
	"S-10",
	"Sierra",
	"SS",
	"Suburban",
	"Tacuma",
	"Tahoe",
	"Tracker Convertible",
	"Tracker Hardtop",
	"Trail Blazer",
	"Trans Sport",
	"Vectra",
	"Venture",
	"Zafira"]

opelModel = "carModel" ~:: oneOfNoCase [
	"Agila",
	"Antara",
	"Astra",
	"Calibra",
	"Combo",
	"Corsa",
	"Frontera",
	"Insignia",
	"Kadett",
	"Meriva",
	"Monterey",
	"Movano",
	"Omega",
	"Signum",
	"Sintra",
	"Tigra",
	"Vectra",
	"Vita",
	"Vivaro",
	"Zafira"]

cadillacModel = "carModel" ~:: oneOfNoCase [
	"Allante",
	"BLS",
	"Brougham",
	"Catera",
	"CTS",
	"DE Ville",
	"DTS",
	"Eldorado",
	"Escalade",
	"Fleetwood",
	"LSE",
	"Seville",
	"SRX",
	"STS",
	"XLR"]

vwModel = "carModel" ~:: oneOfNoCase [
	"Caddy",
	"Amarok",
	"Crafter",
	"T5",
	"Tiguan",
	"Polo",
	"Touareg",
	"Passat",
	"Jetta",
	"Golf",
	"Touran",
	"Phaeton",
	"Eos",
	"Scirocco"]

fordModel = "carModel" ~:: oneOfNoCase [
	"427",
	"Aerostar",
	"Aspire",
	"Bronco",
	"C-Max II",
	"Contour",
	"Cougar",
	"Crown Victoria",
	"Econoline",
	"Escape",
	"Escort",
	"Escort Cabrio",
	"Escort Classic",
	"Escort Estate",
	"Escort Hatchback",
	"Escort Turnier",
	"Escort ZX2",
	"Excursion",
	"Expedition",
	"Explorer",
	"Faction",
	"Falcon GT",
	"Fiesta",
	"Focus",
	"Fusion",
	"Galaxy",
	"Ford GT",
	"Ikon",
	"Ka",
	"Maverick",
	"Model U",
	"Mondeo",
	"Mustang",
	"Probe",
	"Puma",
	"Ranger",
	"Scorpio",
	"Shelby GR",
	"SportKa",
	"StreetKa",
	"Taurus",
	"Thunderbird",
	"Toureno Connect",
	"Transit",
	"Windstar"]

bmwModel = "carModel" ~:: oneOfNoCase [
	"1 series",
	"3 series",
	"5 series",
	"6 series",
	"7 series",
	"8 series",
	"M3",
	"M5",
	"X1",
	"X3",
	"X5",
	"X6",
	"xActivity",
	"Z1",
	"Z3",
	"Z4",
	"Z8"]

hummerModel = "carModel" ~:: oneOfNoCase [
	"H1",
	"H2",
	"H3"]

b2cModel = "carModel" ~:: byteString
chartisModel = "carModel" ~:: byteString

