{-# LANGUAGE OverloadedStrings #-}

-- | Module with model definitions
module Vin.Models (
	runWithDict, runDict,
    models
    ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Char
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.List (find, elem)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Traversable hiding (sequence)

import Vin.Row (column)
import Vin.Model
import Vin.ModelField
import Vin.Models.Cars

-- | File with car models
dictionaries :: FilePath
dictionaries = "resources/site-config/dictionaries/CarModels.json"

getCars :: Value -> String -> [String]
getCars v c = maybe [] (map (T.unpack . label)) $ getMember v ["entries", c]

-- | Dictionaried
type Dict a = Reader Value a

-- | Cars reader
cars :: String -> Dict (ModelField ByteString)
cars s = do
	cars' <- asks (`getCars` s)
	return ("carModel" ~:: oneOfNoCaseByte cars')

-- | Run with dictionary
runWithDict :: FilePath -> Dict a -> IO (Maybe a)
runWithDict f act = fmap (fmap (runReader act)) $ loadValue f

-- | Run default dictionary
runDict :: Dict a -> IO (Maybe a)
runDict = runWithDict dictionaries

wordsF :: [String] -> Text ByteString
wordsF ws = C8.unwords <$> (sequenceA $ map (`typed` byteString) ws)

capitalized :: String -> Text ByteString
capitalized name = (capitalize . C8.uncons) <$> (name `typed` byteString) where
	capitalize Nothing = C8.empty
	capitalize (Just (h, t)) = C8.cons (toUpper h) t

model' :: String -> [ModelRow] -> Model
model' p fs = model p (program p : fs)

mhead :: [ByteString] -> ByteString
mhead [] = C8.empty
mhead (l:_) = l

mheads :: [String] -> String
mheads [] = []
mheads (l:_) = l

withModel
	:: Dict (ModelField ByteString)
	-> (ModelField ByteString -> ModelRow)
	-> String
	-> [ModelRow]
	-> Dict Model
withModel m onModel p fs = do
	m' <- m
	return $ model' p (onModel m' : fs)

ford :: Dict Model
ford = withModel fordModel (<: "MODEL") "ford" [
	carMaker <:= "Ford",
	-- fddsId <: "FDDS_ID",
	companyCode <: "DEALER_CODE",
	companyLATName <: "DEALER_NAME",
	validFrom <: "VALID_FROM",
	validUntil <: "VALID_TO",
	vin <: "VIN_NUMBER",
	plateNumber <: "LICENCE_PLATE_NO",
	--carMake               "carMake",
	-- arcModelCode <: "ARC_MODEL_CODE",
	sellDate <: "FIRST_REGISTRATION_DATE",
	-- fordVehicleType <: "VEHICLE_TYPE",
	-- fordCountryFirstSold <: "COUNTRY_FIRST_SOLD",
	programRegistrationDate <: "CREATION_DATE",
	milageTO <: "MILEAGE"]

fordPlus :: Dict Model
fordPlus = withModel fordModel (<: "Модель") "fordPlus" [
	companyCode <: "UR",
	vin <: "VIN",
	-- carMaker <: "carMake",
	sellDate <: "Дата первой продажи",
	lastTODate <: "Дата прохождения ТО",
	milageTO <: "Пробег на момент прохождения ТО"]

data MotorModel = MotorModel {
	motorModelName :: String,
	motorMotor :: String,
	motorTransmission :: String }
		deriving (Eq, Ord, Read, Show)

vwMotor :: Dict Model
vwMotor = withModel vwModel (<:: (column (encodeString "Модель") vwModelValue)) "vwMotor" [
	carMaker <:= "vw",
	-- vwModel <:: ((head . C8.words) <$> ("Модель" `typed` byteString)),
	-- TODO: Split this column and extract motor & transmission
	color <:: (column (encodeString "Цвет авт") vwColor),
	carMotor <:: (column (encodeString "Модель") vwMotorType <|> pure ""),
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
		vwColor :: TextField ByteString
		vwColor = do
			c <- fieldReader byteString
			let
				res = fst . C8.breakEnd (== '`') . snd . C8.break (== '`') $ c
			return $ if C8.length res < 2 then C8.empty else (C8.init . C8.tail $ res)

		vwModelValue :: TextField ByteString
		vwModelValue = do
			c <- fieldReader byteString
			let failed = throwError $ strMsg $ "Invalid model value " ++ decodeString c
			maybe failed return . listToMaybe . C8.words $ c

		vwMotorType :: TextField ByteString
		vwMotorType = do
			c <- fieldReader byteString
			let
				failed = throwError $ strMsg $ "Can't extract motor " ++ decodeString c
				isMotor s = C8.length s == 3 && (C8.index s 1 `elem` ".,")
			maybe failed return $ find isMotor (C8.words c)

		vwTransmission :: TextField ByteString
		vwTransmission = do
			c <- fieldReader byteString
			let
				failed = throwError $ strMsg $ "Can't extract transmission " ++ decodeString c
				fromTransmission s
					| (encodeString "авт.-") `C8.isPrefixOf` s = Just (encodeString "auto")
					| (encodeString "авт,-") `C8.isPrefixOf` s = Just (encodeString "auto")
					| (encodeString "ручн.-") `C8.isPrefixOf` s = Just (encodeString "mech")
					| (encodeString "ручн,-") `C8.isPrefixOf` s = Just (encodeString "mech")
					| otherwise = Nothing
			maybe failed return $ listToMaybe $ mapMaybe fromTransmission $ C8.words c

vwCommercial :: Dict Model
vwCommercial = withModel vwModel (<:: ((encodeString . mheads . words . decodeString) <$> ("модель" `typed` byteString))) "vwCommercial" [
	carMaker <:= "vw",
	sellDate <: "Дата продажи",
	validUntil <: "Дата окончания карты",
	companyName <: "Продавец",
	cardNumber <: "№ карты",
	vin <: "VIN",
	modelYear <: "модельный год",
	plateNumber <: "госномер",
	ownerName <:: wordsF ["имя", "фамилия", "отчество"],
	ownerAddress <:: wordsF ["адрес частного лица или организации", "город", "индекс"],
	ownerPhone <:: wordsF ["тел1", "тел2"],
	ownerCompany <: "название организации"]

opel :: Dict Model
opel = withModel opelModel (<: "Model") "opel" [
	carMaker <:: (("carMake" `typed` byteString) <|> (pure (encodeString "Opel"))),
	vin <: "VIN",
	carMaker <:: (("Brand" `typed` byteString) <|> pure (encodeString "Opel")),
	companyCode <: "Retail Dealer",
	sellDate <: "Retail Date",
	previousVin <: "Previous VIN (SKD)"]

hummer :: Dict Model
hummer = withModel hummerModel (<:: (dropHummer <$> ("Model" `typed` byteString))) "hummer" [
	companyCode <: "Retail Dealer",
	sellDate <: "Retail Date",
	carMaker <:"Brand",
	vin <: "VIN RUS",
	previousVin <: "VIN"]
	where
		dropHummer = C8.concat . take 1 . drop 1 . C8.words

chevroletNAO :: Dict Model
chevroletNAO = withModel chevroletModel (<: "Model") "chevroletNAO" [
	companyCode <: "Retail Dealer",
	sellDate <: "Retail Date",
	carMaker <: "Brand",
	vin <: "VIN RUS",
	previousVin <: "VIN"]

chevroletKorea :: Dict Model
chevroletKorea = withModel chevroletModel onModel "chevroletKorea" [
	carMaker <:: (("Brand" `typed` carModels) <|> pure (encodeString "")),
	vin <: "VIN",
	companyCode <: "Retail Dealer",
	-- previousVin <: "Previous VIN (SKD)",
	sellDate <: "Retail Date"]
	where
		onModel = (<:: ((encodeString . mheads . words . decodeString) <$> ("Model" `typed` byteString)))

cadillac :: Dict Model
cadillac = withModel cadillacModel onModel "cadillac" [
	companyCode <: "Retail Dealer",
	sellDate <: "Retail Date",
	carMaker <: "Brand",
	vin <: "VIN RUS",
	previousVin <: "VIN"]
	where
		dropCadillac = C8.concat . take 1 . drop 1 . C8.words
		onModel = (<:: (dropCadillac <$> ("Model" `typed` byteString)))

vwRuslan :: Dict Model
vwRuslan = withModel vwModel onModel "vwRuslan" [
	cardNumber <: "№",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	carMaker <:= "vw",
	vin <: "VIN номер Автомобиля VW",
	serviceInterval <: "Межсервисный интервал",
	lastTODate <: "Дата прохождения ТО (Дата регистрации в программе)",
	milageTO <: "Величина пробега на момент регистрации в Программе",
	validUntil <: "Программа действует до (Дата)"]
	where
		onModel = (<:: (rusVW <$> ("Модель Автомобиля VW" `typed` byteString)))

chartis :: Dict Model
chartis = withModel chartisModel (<:: capitalized "Модель Автомобиля") "chartis" [
	cardNumber <: "№",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	carMaker <:: capitalized "Марка Автомобиля",
	vin <: "VIN номер Автомобиля",
	validFrom <: "Дата регистрации в программе",
	validUntil <: "Программа действует до (Дата)"]

vwAvilon :: Dict Model
vwAvilon = withModel vwModel (<: "Модель Автомобиля VW") "vwAvilon" [
	carMaker <:= "vw",
	cardNumber <: "Подрядковый номер клубной карты",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	vin <: "VIN номер Автомобиля VW",
	serviceInterval <: "Межсервисный интервал",
	validFrom <: "Дата регистрации в программе",
	milageTO <: "Величина пробега на момент регистрации в Программе",
	validUntil <: "Программа действует до (Дата)"]

atlantM :: Dict Model
atlantM = withModel vwModel (<: "Модель Автомобиля VW") "atlant" [
	carMaker <:= "vw",
	cardNumber <: "Номер карты Atlant-M Assistance",
	subProgramName <: "Тип программы",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	vin <: "VIN номер Автомобиля VW",
	serviceInterval <: "Межсервисный интервал, км",
	validFrom <: "Дата регистрации в программе",
	milageTO <: "Величина пробега на момент регистрации в Программе, км",
	validUntil <: "Программа действует до (Дата)"]

autocraft :: Dict Model
autocraft = withModel bmwModel (<: "Модель Автомобиля BMW") "autocraft" [
	carMaker <:= "BMW",
	cardNumber <: "Подрядковый номер клубной карты",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	ownerName <: "ФИО Клиента",
	vin <: "VIN номер Автомобиля BMW (последние 7 знаков)",
	validFrom <: "Дата регистрации в программе",
	validUntil <: "Программа действует до (Даты)",
	milageTO <: "Величина пробега на момент регистрации в Программе"]

b2c :: Dict Model
b2c = withModel b2cModel (<: "Модель автомобиля") "b2c" [
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
	modelYear <: "Год выпуска",
	plateNumber <: "Гос номер",
	vin <: "Идентификационный номер (VIN)",
	ownerContact <:: wordsF ["Фамилия доверенного лица", "Имя доверенного лица", "Отчество доверенного лица"]]

models :: Dict [Model]
models = sequence [ford, fordPlus, vwMotor, vwCommercial, opel, hummer, chevroletNAO,
	chevroletKorea, cadillac, vwRuslan, chartis, vwAvilon, atlantM, autocraft,
	b2c]

chevroletModel :: Dict (ModelField ByteString)
opelModel :: Dict (ModelField ByteString)
cadillacModel :: Dict (ModelField ByteString)
vwModel :: Dict (ModelField ByteString)
fordModel :: Dict (ModelField ByteString)
bmwModel :: Dict (ModelField ByteString)
hummerModel :: Dict (ModelField ByteString)
b2cModel :: Dict (ModelField ByteString) -- FIXME: Is this correct?
chartisModel :: Dict (ModelField ByteString) -- FIXME: Is this correct?

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

chevroletModel = cars "chevy"
opelModel = cars "opel"
cadillacModel = cars "cad"
vwModel = cars "vw"
fordModel = cars "ford"
bmwModel = cars "bmw"
hummerModel = cars "hum"
b2cModel = return ("carModel" ~:: byteString)
chartisModel = return ("carModel" ~:: byteString)

