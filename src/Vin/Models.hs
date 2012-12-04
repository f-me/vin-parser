{-# LANGUAGE OverloadedStrings #-}

-- | Module with model definitions
module Vin.Models (
	runWithDicts, runDict,
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
import Data.List (find)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable hiding (sequence, forM)

import Vin.Row (column)
import Vin.Model
import Vin.ModelField
import Vin.Models.Cars

-- | File with car models
dictionaries :: FilePath
dictionaries = "resources/site-config/dictionaries/CarModels.json"

makers :: FilePath
makers = "resources/site-config/dictionaries/CarMakers.json"

programs :: FilePath
programs = "resources/site-config/dictionaries/Programs.json"

colors :: FilePath
colors = "resources/site-config/dictionaries/Colors.json"

getCars :: Value -> String -> [String]
getCars v c = maybe [] (map (T.unpack . label)) $ getMember v ["entries", c]

getCarsReverseMap :: Value -> String -> M.Map ByteString ByteString
getCarsReverseMap v c = maybe M.empty (M.unions . map mk) $ getMember v ["entries", c] where
	mk e = M.singleton (T.encodeUtf8 . label $ e) (T.encodeUtf8 . value $ e)

getReverseMap :: Value -> M.Map ByteString ByteString
getReverseMap v = maybe M.empty (M.unions . map mk) $ getMember v ["entries"] where
	mk e = M.singleton (T.encodeUtf8 . label $ e) (T.encodeUtf8 . value $ e)

getMakers :: Value -> M.Map ByteString ByteString
getMakers = getReverseMap

getPrograms :: Value -> [String]
getPrograms v = maybe [] (map (T.unpack . value)) $ getMember v ["entries"]

getColors :: Value -> M.Map ByteString ByteString
getColors = getReverseMap

data CarDictionaries = CarDictionaries {
	modelsDict :: Value,
	makersDict :: Value,
	programsDict :: Value,
	colorsDict :: Value }
		deriving (Eq, Show)

-- | Dictionaried
type Dict a = Reader CarDictionaries a

-- | Cars reader
cars :: String -> Dict (ModelField ByteString)
cars s = do
	cars' <- asks ((`getCarsReverseMap` s) . modelsDict)
	return ("model" ~:: tableLowCase cars')

-- | Gets model-value by make-label and model-label
allCars :: Dict (M.Map ByteString (M.Map ByteString ByteString))
allCars = do
	mk' <- asks (getMakers . makersDict)
	fmap M.unions $ forM (M.toList mk') $ \(mkLabel, mkValue) -> do
		cs' <- asks ((`getCarsReverseMap` (T.unpack . T.decodeUtf8 $ mkValue)) . modelsDict)
		return $ M.singleton mkLabel cs'

-- | Cars reader
carsList :: String -> Dict (ModelField ByteString)
carsList s = do
	cars' <- asks ((`getCars` s) . modelsDict)
	return ("model" ~:: oneOfNoCaseByte cars')

makersTable :: Dict (ModelField ByteString)
makersTable = do
	mk' <- asks (getMakers . makersDict)
	return ("make" ~:: tableLowCase mk')

programsList :: Dict [String]
programsList = asks (getPrograms . programsDict)

colorsTable :: Dict (ModelField ByteString)
colorsTable = do
	c' <- asks (getColors . colorsDict)
	return ("color" ~:: tableLowCase c')

-- | Run with dictionary
runWithDicts :: FilePath -> FilePath -> FilePath -> FilePath -> Dict a -> IO (Maybe a)
runWithDicts fc fm fp fcl act = do
	c <- loadValue fc
	m <- loadValue fm
	p <- loadValue fp
	cl <- loadValue fcl
	return $ fmap (runReader act) $ CarDictionaries <$> c <*> m <*> p <*> cl
-- runWithDicts f act = fmap (fmap (runReader act)) $ loadValue f

-- | Run default dictionary
runDict :: Dict a -> IO (Maybe a)
runDict = runWithDicts dictionaries makers programs colors

wordsF :: [String] -> Text ByteString
wordsF ws = C8.unwords <$> sequenceA (map (`typed` byteString) ws)

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
	carMaker <:= "ford",
	-- fddsId <: "FDDS_ID",
	companyCode <: "DEALER_CODE",
	companyLATName <: "DEALER_NAME",
	checkupDate <: "VALID_FROM",
	vin <: "VIN_NUMBER",
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
	carMaker <:= "ford",
	sellDate <: "Дата первой продажи",
	lastTODate <: "Дата прохождения ТО",
	milageTO <: "Пробег на момент прохождения ТО"]

data MotorModel = MotorModel {
	motorModelName :: String,
	motorMotor :: String,
	motorTransmission :: String }
		deriving (Eq, Ord, Read, Show)

vwMotor :: Dict Model
vwMotor = withModel vwModel (<:: column (encodeString "Модель") vwModelValue) "vwMotor" [
	carMaker <:= "vw",
	-- vwModel <:: ((head . C8.words) <$> ("Модель" `typed` byteString)),
	-- TODO: Split this column and extract motor & transmission
	color <:: column (encodeString "Цвет авт") vwColor,
	carMotor <:: (column (encodeString "Модель") vwMotorType <|> pure ""),
	carTransmission <:: column (encodeString "Модель") vwTransmission,
	makeYear <: "Модельный год",
	vin <: "VIN",
	dealerCode <: "Код дилера получателя",
	companyName <: "Дилер получатель",
	contractNo <: "No Дог продажи Клиенту",
	buyDate <: "Дата договора продажи",
	ownerCompany <: "Компания покупатель",
	ownerContact <: "Контактное лицо покупателя",
	ownerName <: "Фактический получатель ам"]
	where
		vwColor :: TextField ByteString
		vwColor = do
			c <- fieldReader byteString
			let
				res = fst . C8.breakEnd (== '`') . snd . C8.break (== '`') $ c
			return $ if C8.length res < 2 then C8.empty else C8.init . C8.tail $ res

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
					| encodeString "авт.-" `C8.isPrefixOf` s = Just (encodeString "auto")
					| encodeString "авт,-" `C8.isPrefixOf` s = Just (encodeString "auto")
					| encodeString "ручн.-" `C8.isPrefixOf` s = Just (encodeString "mech")
					| encodeString "ручн,-" `C8.isPrefixOf` s = Just (encodeString "mech")
					| otherwise = Nothing
			maybe failed return $ listToMaybe $ mapMaybe fromTransmission $ C8.words c

vwCommercial :: Dict Model
vwCommercial = withModel vwModel (<:: ((encodeString . mheads . words . decodeString) <$> ("модель" `typed` byteString))) "vwcargo" [
	carMaker <:= "vw",
	sellDate <: "Дата продажи",
	validUntil <: "Дата окончания карты",
	companyName <: "Продавец",
	cardNumber <: "№ карты",
	vin <: "VIN",
	makeYear <: "модельный год",
	plateNumber <: "госномер",
	ownerName <:: wordsF ["имя", "фамилия", "отчество"],
	ownerAddress <:: wordsF ["адрес частного лица или организации", "город", "индекс"],
	ownerPhone <:: wordsF ["тел1", "тел2"],
	ownerCompany <: "название организации"]

opel :: Dict Model
opel = withModel opelModel (<: "Model") "opel" [
	carMaker <:= "opel",
	vin <:: (("VIN" `typed` modelFieldType vin) <|> ("Previous VIN (SKD)" `typed` modelFieldType vin)),
	carMaker <:: (("Brand" `typed` byteString) <|> pure (encodeString "Opel")),
	seller <: "Retail Dealer",
	sellDate <: "Retail Date"]

hummer :: Dict Model
hummer = withModel hummerModel (<:: (dropHummer <$> ("Model" `typed` byteString))) "hum" [
	seller <: "Retail Dealer",
	sellDate <: "Retail Date",
	carMaker <:= "hum",
	vin <: "VIN RUS",
	previousVin <: "VIN"]
	where
		dropHummer = C8.concat . take 1 . drop 1 . C8.words

chevroletNAO :: Dict Model
chevroletNAO = withModel chevroletModel (<: "Model") "chevyna" [
	seller <: "Retail Dealer",
	sellDate <: "Retail Date",
	carMaker <:= "chevy",
	vin <: "VIN RUS",
	previousVin <: "VIN"]

chevroletKorea :: Dict Model
chevroletKorea = withModel chevroletModel onModel "chevyko" [
	carMaker <:= "chevy",
	vin <: "VIN",
	seller <: "Retail Dealer",
	-- previousVin <: "Previous VIN (SKD)",
	sellDate <: "Retail Date"]
	where
		onModel = (<:: ((encodeString . mheads . words . decodeString) <$> ("Model" `typed` byteString)))

cadillac :: Dict Model
cadillac = withModel cadillacModel onModel "cadold" [
	seller <: "Retail Dealer",
	sellDate <: "Retail Date",
	carMaker <:= "cad",
	vin <: "VIN RUS",
	previousVin <: "VIN"]
	where
		dropCadillac = C8.concat . take 1 . drop 1 . C8.words
		onModel = (<:: (dropCadillac <$> ("Model" `typed` byteString)))

vwRuslan :: Dict Model
vwRuslan = withModel vwModel onModel "ruslan" [
	cardNumber <: "№",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	carMaker <:= "vw",
	vin <: "VIN номер Автомобиля VW",
	serviceInterval <: "Межсервисный интервал",
	lastTODate <: "Дата прохождения ТО (Дата регистрации в программе)",
	milageTO <: "Величина пробега на момент регистрации в Программе",
	validUntil <: "Программа действует до (Дата)",
	validUntilMilage <: "Программа действует до (Пробега)"]
	where
		onModel = (<:: ("Модель Автомобиля VW" `typed` rusModel))
		-- onModel = (<:: (rusVW <$> ("Модель Автомобиля VW" `typed` byteString)))

chartis :: Dict Model
chartis = withModel chartisModel (<:: capitalized "Модель Автомобиля") "chartis" [
	cardNumber <: "Подрядковый номер клубной карты",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	carMaker <:: capitalized "Марка Автомобиля",
	vin <: "VIN номер Автомобиля",
	validFrom <: "Дата регистрации в программе",
	validUntil <: "Программа действует до (Дата)"]

vwAvilon :: Dict Model
vwAvilon = withModel vwModel (<: "Модель Автомобиля VW") "avilon" [
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
	programName <: "Тип программы",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	vin <: "VIN номер Автомобиля VW",
	serviceInterval <: "Межсервисный интервал, км",
	validFrom <: "Дата регистрации в программе",
	milageTO <: "Величина пробега на момент регистрации в Программе, км",
	validUntil <: "Программа действует до (Дата)"]

autocraft :: Dict Model
autocraft = withModel bmwModel (<: "Модель Автомобиля BMW") "autocraft" [
	carMaker <:= "bmw",
	cardNumber <: "Подрядковый номер клубной карты",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл",
	ownerName <: "ФИО Клиента",
	vin <: "VIN номер Автомобиля BMW (последние 7 знаков)",
	validFrom <: "Дата регистрации в программе",
	validUntil <: "Программа действует до (Даты)",
	milageTO <: "Величина пробега на момент регистрации в Программе"]

europlan :: Dict Model
europlan = withModel europlanModel (<: "Модель автомобиля") "euro" [
	carMaker <: "Марка автомобиля",
	cardNumber <: "Подрядковый номер клубной карты",
	makeYear <: "Год выпуска автомобиля",
	vin <: "VIN номер автомобиля",
	validFrom <: "Дата регистрации в программе",
	programName <: "Тип программы",
	validUntil <: "Программа действует до (Дата)",
	manager <: "ФИО ответственного лица, внесшего данные в XLS файл"]

b2c :: Dict Model
b2c = withModel b2cModel (<: "Модель автомобиля") "b2c" [
	validFrom <: "Дата активации карты",
	validUntil <: "Дата окончания срока дейсвия карты",
	manager <: "Сотрудник РАМК",
	cardNumber <: "Номер карты",
	programName <: "Тип карты",
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
	makeYear <: "Год выпуска",
	plateNumber <: "Гос номер",
	vin <: "Идентификационный номер (VIN)",
	ownerContact <:: wordsF ["Фамилия доверенного лица", "Имя доверенного лица", "Отчество доверенного лица"]]

universal :: Dict (String -> Model)
universal = do
	fmake <- makersTable
	col <- colorsTable
	allCarModels <- allCars
	let
		lookupModel mk mdl = fromMaybe "" $ do
			mkDict <- M.lookup mk allCarModels
			M.lookup mdl mkDict
		checkModel :: FieldType ByteString
		checkModel = verifyType (not . C8.null) "Invalid car model" byteString
	return $ \p -> model' p [
		vin <: "VIN",
		("owner_name" ~:: byteString) <: "ФИО владельца",
		("owner_phone1" ~:: byteString) <: "Контактный телефон владельца",
		("owner_email" ~:: email) <: "Email владельца",
		fmake <: "Марка",
		("model" ~:: checkModel) <:: (lookupModel <$> ("Марка" `typed` byteString) <*> ("Модель" `typed` byteString)),
		plateNumber <: "Госномер",
		makeYear <: "Год производства автомобиля",
		col <: "Цвет",
		buyDate <: "Дата покупки",
		checkupDate <: "Дата последнего ТО",
		cardNumber <: "Номер карты участника",
		validFrom <: "Дата регистрации в программе",
		validUntil <: "Программа действует до (дата)",
		validUntilMilage <: "Программа действует до (пробег)",
		milageTO <: "Пробег при регистрации в программе",
		serviceInterval <: "Межсервисный интервал",
		("cardOwner" ~:: byteString) <: "ФИО владельца карты",
		manager <: "ФИО менеджера"]

models :: Dict (String -> Maybe Model)
models = do
	m <- sequence [ford, fordPlus, vwMotor, vwCommercial, opel, hummer, chevroletNAO, chevroletKorea, cadillac, vwRuslan, chartis, vwAvilon, atlantM, autocraft, europlan, b2c]
	u <- universal
	ps <- programsList
	return $ \p -> find ((== p) . modelProgram) m <|> fmap u (find (== p) ps)

chevroletModel :: Dict (ModelField ByteString)
opelModel :: Dict (ModelField ByteString)
cadillacModel :: Dict (ModelField ByteString)
vwModel :: Dict (ModelField ByteString)
fordModel :: Dict (ModelField ByteString)
bmwModel :: Dict (ModelField ByteString)
hummerModel :: Dict (ModelField ByteString)
europlanModel :: Dict (ModelField ByteString)
b2cModel :: Dict (ModelField ByteString) -- FIXME: Is this correct?
chartisModel :: Dict (ModelField ByteString) -- FIXME: Is this correct?

-- Synonyms
rusTable :: M.Map ByteString ByteString
rusTable = M.unions [
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

-- rusModel
rusModel :: FieldType ByteString
rusModel = table rusTable

-- Try map russian names of VW
rusVW :: ByteString -> ByteString
rusVW s = fromMaybe s $ M.lookup s rusTable

chevroletModel = cars "chevy"
opelModel = cars "opel"
cadillacModel = cars "cad"
vwModel = cars "vw"
fordModel = cars "ford"
bmwModel = cars "bmw"
hummerModel = cars "hum"
europlanModel = return ("model" ~:: byteString)
b2cModel = return ("model" ~:: byteString)
chartisModel = return ("model" ~:: byteString)

arcModelCode             = "modelCode"                      ~:: byteString
buyDate                  = "buyDate"                        ~:: time
callTaker                 = "callTaker"                      ~:: byteString
carMaker                 = "make"                           ~:: carMakers
carModel                 = "model"                          ~:: carModels
carMotor                 = "motor"                          ~:: byteString
carTransmission          = "transmission"                   ~:: byteString
cardNumber               = "cardNumber"                     ~:: byteString
checkupDate              = "checkupDate"                    ~:: time
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
makeYear                = "makeYear"                      ~:: int
ownerAddress             = "ownerAddress"                   ~:: byteString
ownerCompany             = "ownerCompany"                   ~:: byteString
ownerContact             = "ownerContact"                   ~:: byteString
ownerEmail               = "ownerEmail"                     ~:: email
ownerLATName             = "ownerLATName"                   ~:: byteString
ownerName                = "ownerName"                      ~:: byteString
ownerPhone               = "ownerPhone"                     ~:: phone
plateNumber              = "plateNum"                       ~:: byteString
previousVin              = "vin2"                           ~:: upperByteString
programName              = "program"                        ~:: byteString
programRegistrationDate  = "programRegistrationDate"        ~:: time
sellDate                 = "buyDate"                        ~:: time
seller                   = "seller"                         ~:: byteString
serviceInterval          = "serviceInterval"                ~:: int
subProgramName           = "subProgramName"                 ~:: byteString
validFrom                = "validFrom"                      ~:: time
validUntil               = "validUntil"                     ~:: time
validUntilMilage         = "validUntilMilage"               ~:: int
vin                      = "vin"                            ~:: notNull upperByteString
