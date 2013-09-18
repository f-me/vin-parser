{-# LANGUAGE OverloadedStrings #-}

module Vin.Models (
    runWithDicts, runDict,
    models
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Char (toUpper, toLower)
import Data.List (find)
import Data.Maybe (listToMaybe, fromMaybe, mapMaybe)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (sequenceA)

import Vin.Field
import Vin.Model
import Vin.Models.Cars

-- | Dictionary with car models
dictionaries :: FilePath
dictionaries = "resources/site-config/dictionaries/CarModels.json"

-- | Dictionary with car makers
makers :: FilePath
makers = "resources/site-config/dictionaries/CarMakers.json"

-- | Dictionary with formats
formats :: FilePath
formats = "resources/site-config/dictionaries/VinFormats.json"

-- | Dictionary with colors
colors :: FilePath
colors = "resources/site-config/dictionaries/Colors.json"

-- | Dictionaries loaded from JSON
data CarDictionaries = CarDictionaries {
    makersDict :: M.Map Text Text,
    -- ^ @Map@ from maker label to maker value
    modelsDict :: M.Map Text (M.Map Text Text),
    -- ^ @Map@ from maker value to @Map@ from model label to model value
    formatsList :: [String],
    -- ^ List of formats
    colorsDict :: M.Map Text Text }
    -- ^ @Map@ from color label to color value

-- | Dictionaried
type Dict a = Reader CarDictionaries a

-- | Load subdictionary from @Value@
getSubMap :: Value -> [String] -> M.Map Text Text
getSubMap v sub = maybe M.empty (M.unions . map mk) $ getMember v ("entries" : sub) where
    mk e = M.singleton (value e) (label e)

-- | Get top-level dictionary from @Value@
getMap :: Value -> M.Map Text Text
getMap v = getSubMap v []

-- | Make @Map@ from value to key
reverseMap :: (Ord k, Ord a) => M.Map k a -> M.Map a k
reverseMap = M.fromList . map (snd &&& fst) . M.toList

-- | Load dicts and run action
runWithDicts :: FilePath -> FilePath -> FilePath -> FilePath -> Dict a -> IO (Maybe a)
runWithDicts fm fc fp fcl act = do
    m <- fmap (fmap (reverseMap . getMap)) $ loadValue fm
    c <- loadValue fc
    let
        getCarModels :: Value -> Text -> M.Map Text (M.Map Text Text)
        getCarModels v make = M.singleton make (reverseMap $ getSubMap v [T.unpack make])
        mdls = do
            m' <- m
            c' <- c
            return $ M.unions $ map (getCarModels c') (M.elems m')
    p <- fmap (fmap (map T.unpack . M.keys . getMap)) $ loadValue fp
    cl <- fmap (fmap (reverseMap . getMap)) $ loadValue fcl
    return $ fmap (runReader act) $ liftM4 CarDictionaries m mdls p cl

-- | Run with default dictionaries
runDict :: Dict a -> IO (Maybe a)
runDict = runWithDicts makers dictionaries formats colors

-- | Get car models @Map@ from label to value for maker specified
getCarModels :: Text -> Dict (M.Map Text Text)
getCarModels make = do
    mdls <- asks modelsDict
    maybe (return M.empty) return $ M.lookup make mdls

-- | Get make field parser and model field parser
makeModelParser :: Dict (String -> Field Text, String -> String -> Field Text)
makeModelParser = do
    makers <- asks makersDict
    models <- asks modelsDict
    return (tableField makers, modelField makers models)

-- | Make model by its name
models :: Dict (String -> Maybe Model)
models = do
    m <- sequence [
        ford,
        fordPlus,
        vwMotor,
        vwCommercial,
        opel,
        hummer,
        chevroletNAO,
        chevroletKorea,
        cadillac,
        vwRuslan,
        chartis,
        vwAvilon,
        atlantM,
        autocraft,
        europlan,
        b2c,
        vtb24,
        citroenPeugeot "citroen",
        citroenPeugeot "peugeot"]
    u <- universal
    fs <- asks formatsList
    return $ \p -> find ((== p) . modelProgram) m <|> fmap u (find (== p) fs)

-- Predefined fields

car_buyDate = ("carBuyDate", time)
car_checkupDate = ("carCheckupDate", time)
car_checkupMileage = ("carCheckupMilage", int)
car_color = ("carColor", text)
car_make = ("carMake", text)
car_makeYear = ("carMakeYear", int)
car_model = ("carModel", text)
car_plateNum = ("carPlateNum", text)
car_seller = ("carSeller", text)
car_transmission = ("carTransmission", text)
car_vin = ("carVin", vin)
car_warrantyEnd = ("warrantyEnd", time)
car_warrantyStart = ("warrantyStart", time)
cardNumber = ("cardNumber", text)
cardOwner = ("cardOwner", text)
manager = ("manager", text)
milageTO = ("milageTO", int)
program = ("program", text)
validFrom = ("contractValidFromDate", time)
validUntil = ("contractValidUntilDate", time)
validUntilMilage = ("contractValidUntilMilage", int)

-- Fields not present in contract model
-- car_motor = ("car_motor", text)
modelCode = ("modelCode", text)
ownerEmail = ("contact_ownerEmail", email)
ownerName = ("contact_ownerName", text)
ownerPhone = ("contact_ownerPhone1", text)
serviceInterval = ("cardNumber_serviceInterval", int)

-- Helpers

-- | Words from several fields
wordsFrom :: [String] -> Field Text
wordsFrom = fmap T.unwords . sequenceA . map tryField

-- | Capitalize text
capitalized :: Text -> Text
capitalized str = case T.uncons str of
    Nothing -> T.empty
    Just (h, t) -> T.cons (toUpper h) (T.map toLower t)

-- | Take first word
firstWord :: Field Text -> Field Text
firstWord fld = fld >>= maybe empty return . listToMaybe . T.words

-- | Take second word
secondWord :: Field Text -> Field Text
secondWord fld = fld >>= maybe empty return . listToMaybe . drop 1 . T.words

-- | Car model value by make label and model label
modelField :: M.Map Text Text -> M.Map Text (M.Map Text Text) -> String -> String -> Field Text
modelField makers models fmake fmodel = do
    makeValue <- tableField makers fmake
    let
        modelTable = fromMaybe M.empty $ M.lookup makeValue models
    tableField modelTable fmodel

-- Models

ford :: Dict Model
ford = do
    fords <- getCarModels "ford"
    return $ model "ford" [
        car_buyDate <~ tryField "FIRST_REGISTRATION_DATE",
        car_checkupDate <~ tryField "VALID_FROM",
        car_checkupMileage <~ tryField "MILEAGE",
        car_make <~ pure "ford",
        car_model <~ tableField fords "MODEL",
        car_seller <~ tryField "DEALER_NAME",
        car_vin <~ field "VIN_NUMBER"]

fordPlus :: Dict Model
fordPlus = do
    fords <- getCarModels "ford"
    return $ model "fordPlus" [
        car_buyDate <~ tryField "Дата первой продажи",
        car_checkupDate <~ tryField "Дата прохождения ТО",
        car_checkupMileage <~ tryField "Пробег на момент прохождения ТО",
        car_make <~ pure "ford",
        car_model <~ tableField fords "Модель",
        car_vin <~ field "VIN"]

vwMotor :: Dict Model
vwMotor = do
    vws <- getCarModels "vw"
    cols <- asks colorsDict
    return $ model "vwMotor" [
        car_make <~ pure "vw",
        car_model <~ with (firstWord . tryField) "Модель" (look vws),
        car_color <~ with (takeColor . tryField) "Цвет авт" (lookKey cols),
        -- car_motor <~ with tryField "Модель" takeMotor,
        car_transmission <~ with_ tryField "Модель" takeTransmission,
        car_makeYear <~ tryField "Модельный год",
        car_vin <~ field "VIN",
        car_seller <~ tryField "Дилер получатель",
        car_buyDate <~ tryField "Дата договора продажи",
        ownerName <~ tryField "Контактное лицо покупателя"]
    where
        takeColor :: Field Text -> Field Text
        takeColor fld = fld >>= maybe empty return . fmap camelCase . takeColor' where
            takeColor' s = case fst . T.breakOnEnd "`" . snd . T.breakOn "`" $ s of
                res
                    | T.length res < 2 -> Nothing
                    | otherwise -> Just $ T.init $ T.tail res
            camelCase :: T.Text -> T.Text
            camelCase color = case T.words color of
                [] -> T.empty
                (c : cs) -> T.concat (T.map toLower c : map upperCase cs)
            upperCase :: T.Text -> T.Text
            upperCase = maybe T.empty (\(c, cs) -> T.cons (toUpper c) (T.map toLower cs)) . T.uncons

        takeMotor :: Text -> Either String Text
        takeMotor str = maybe err return . find isMotor . T.words $ str where
            isMotor s = T.length s == 3 && (T.index s 1 `elem` ".,")
            err = Left $ "Невозможно определить car_motor из '" ++ T.unpack str ++ "'"

        takeTransmission :: Text -> Maybe Text
        takeTransmission str = listToMaybe . mapMaybe fromTransmission . T.words $ str where
            fromTransmission s
                | "авт.-" `T.isPrefixOf` s = Just "auto"
                | "авт,-" `T.isPrefixOf` s = Just "auto"
                | "ручн.-" `T.isPrefixOf` s = Just "mech"
                | "ручн,-" `T.isPrefixOf` s = Just "mech"
                | "-мест" `T.isInfixOf` s = Just "mech"
                | otherwise = Nothing

        -- | Check whether key exists
        lookKey :: M.Map Text Text -> Text -> Either String Text
        lookKey tbl key = if key `elem` M.elems tbl then return key else err where
            err = Left $ "Неверное значение: '" ++ T.unpack key ++ "'"

vwCommercial :: Dict Model
vwCommercial = do
    vws <- getCarModels "vw"
    return $ model "vwcargo" [
        car_buyDate <~ tryField "Дата продажи",
        car_make <~ pure "vw",
        car_makeYear <~ tryField "модельный год",
        car_model <~ with (firstWord . tryField) "модель" (look vws),
        car_plateNum <~ tryField "госномер",
        car_seller <~ tryField "Продавец",
        car_vin <~ field "VIN",
        cardNumber <~ tryField "№ карты",
        ownerName <~ wordsFrom ["имя", "фамилия", "отчество"],
        ownerPhone <~ wordsFrom ["тел1", "тел2"],
        validUntil <~ tryField "Дата окончания карты"]

opel :: Dict Model
opel = do
    opels <- getCarModels "opel"
    return $ model "opel" [
        car_buyDate <~ tryField "Retail Date",
        car_make <~ pure "opel",
        car_model <~ tableField opels "Model",
        car_seller <~ tryField "Retail Dealer",
        car_vin <~ (field "VIN" <|> field "Previous VIN (SKD)")]

hummer :: Dict Model
hummer = do
    hums <- getCarModels "hum"
    return $ model "hum" [
        car_buyDate <~ tryField "Retail Date",
        car_make <~ pure "hum",
        car_model <~ with (secondWord . tryField) "Model" (look hums),
        car_seller <~ tryField "Retail Dealer",
        car_vin <~ field "VIN RUS"]

chevroletNAO :: Dict Model
chevroletNAO = do
    chevys <- getCarModels "chevy"
    return $ model "chevyna" [
        car_buyDate <~ tryField "Retail Date",
        car_make <~ pure "chevy",
        car_model <~ tableField chevys "Model",
        car_seller <~ tryField "Retail Dealer",
        car_vin <~ field "VIN RUS"]

chevroletKorea :: Dict Model
chevroletKorea = do
    chevys <- getCarModels "chevy"
    return $ model "chevyko" [
        car_buyDate <~ tryField "Retail Date",
        car_make <~ pure "chevy",
        car_model <~ with (firstWord . tryField) "Model" (look chevys),
        car_seller <~ tryField "Retail Dealer",
        car_vin <~ field "VIN"]

cadillac :: Dict Model
cadillac = do
    cads <- getCarModels "cad"
    return $ model "cadold" [
        car_buyDate <~ field "Retail Date",
        car_make <~ pure "cad",
        car_model <~ with (secondWord . field) "Model" (look cads),
        car_seller <~ field "Retail Dealer",
        car_vin <~ field "VIN RUS",
        cardNumber <~ field "Номер карты"]

vwRuslan :: Dict Model
vwRuslan = do
    vws <- getCarModels "vw"
    return $ model "ruslan" [
        car_make <~ pure "vw",
        car_model <~ with tryField "Модель Автомобиля VW" (look vws),
        car_vin <~ field "VIN номер Автомобиля VW",
        cardNumber <~ tryField "№",
        manager <~ tryField "ФИО ответственного лица, внесшего данные в XLS файл",
        milageTO <~ tryField "Величина пробега на момент регистрации в Программе",
        validFrom <~ tryField "Дата прохождения ТО (Дата регистрации в программе)",
        validUntil <~ tryField "Программа действует до (Дата)",
        validUntilMilage <~ tryField "Программа действует до (Пробега)"]

chartis :: Dict Model
chartis = do
    (makeFld, modelFld) <- makeModelParser
    return $ model "chartis" [
        car_make <~ makeFld "Марка Автомобиля",
        car_model <~ modelFld "Марка Автомобиля" "Модель Автомобиля",
        car_vin <~ field "VIN номер Автомобиля",
        cardNumber <~ tryField "Подрядковый номер клубной карты",
        manager <~ tryField "ФИО ответственного лица, внесшего данные в XLS файл",
        validFrom <~ tryField "Дата регистрации в программе",
        validUntil <~ tryField "Программа действует до (Дата)"]

vwAvilon :: Dict Model
vwAvilon = do
    vws <- getCarModels "vw"
    return $ model "avilon" [
        car_make <~ pure "vw",
        car_model <~ tableField vws "Модель Автомобиля VW",
        car_vin <~ field "VIN номер Автомобиля VW",
        cardNumber <~ tryField "Подрядковый номер клубной карты",
        manager <~ tryField "ФИО ответственного лица, внесшего данные в XLS файл",
        milageTO <~ tryField "Величина пробега на момент регистрации в Программе",
        serviceInterval <~ tryField "Межсервисный интервал",
        validFrom <~ tryField "Дата регистрации в программе",
        validUntil <~ tryField "Программа действует до (Дата)"]

atlantM :: Dict Model
atlantM = do
    vws <- getCarModels "vw"
    return $ model "atlant" [
        car_make <~ pure "vw",
        car_model <~ tableField vws "Модель Автомобиля VW",
        car_vin <~ field "VIN номер Автомобиля VW",
        cardNumber <~ tryField "Номер карты Atlant-M Assistance",
        manager <~ tryField "ФИО ответственного лица, внесшего данные в XLS файл",
        milageTO <~ tryField "Величина пробега на момент регистрации в Программе, км",
        -- program <~ field "Тип программы", -- ???
        serviceInterval <~ tryField "Межсервисный интервал, км",
        validFrom <~ tryField "Дата регистрации в программе",
        validUntil <~ tryField "Программа действует до (Дата)"]

autocraft :: Dict Model
autocraft = do
    bmws <- getCarModels "bmw"
    return $ model "autocraft" [
        car_make <~ pure "bmw",
        car_model <~ tableField bmws "Модель Автомобиля BMW",
        car_vin <~ field "VIN номер Автомобиля BMW (последние 7 знаков)",
        cardNumber <~ tryField "Подрядковый номер клубной карты",
        manager <~ tryField "ФИО ответственного лица, внесшего данные в XLS файл",
        milageTO <~ tryField "Величина пробега на момент регистрации в Программе",
        ownerName <~ tryField "ФИО Клиента",
        validFrom <~ tryField "Дата регистрации в программе",
        validUntil <~ tryField "Программа действует до (Даты)"]

europlan :: Dict Model
europlan = do
    (makeFld, modelFld) <- makeModelParser
    return $ model "euro" [
        car_make <~ makeFld "Марка автомобиля",
        car_makeYear <~ tryField "Год выпуска автомобиля",
        car_model <~ modelFld "Марка автомобиля" "Модель автомобиля",
        car_vin <~ field "VIN номер автомобиля",
        cardNumber <~ tryField "Подрядковый номер клубной карты",
        manager <~ tryField "ФИО ответственного лица, внесшего данные в XLS файл",
        -- program <~ field "Тип программы", -- ???
        validFrom <~ tryField "Дата регистрации в программе",
        validUntil <~ tryField "Программа действует до (Дата)"]

b2c :: Dict Model
b2c = do
    (makeFld, modelFld) <- makeModelParser
    return $ model "b2c" [
        car_make <~ makeFld "Марка автомобиля",
        car_makeYear <~ tryField "Год выпуска",
        car_model <~ modelFld "Марка автомобиля" "Модель автомобиля",
        car_plateNum <~ tryField "Гос номер",
        car_vin <~ field "Идентификационный номер (VIN)",
        cardNumber <~ tryField "Номер карты",
        manager <~ tryField "Сотрудник РАМК",
        ownerEmail <~ tryField "Е-МAIL клиента",
        ownerName <~ wordsFrom ["Фамилия клиента", "Имя клиента", "Отчество клиента"],
        ownerPhone <~ wordsFrom ["Телефон клиента Мобильный", "Телефон клиента Домашний"],
        -- program <~ tryField "Тип карты", -- ???
        validFrom <~ tryField "Дата активации карты",
        validUntil <~ tryField "Дата окончания срока дейсвия карты"]

citroenPeugeot :: String -> Dict Model
citroenPeugeot progname = do
    (makeFld, modelFld) <- makeModelParser
    return $ model progname [
        car_buyDate <~ tryField "FIRST_REGISTRATION_DATE",
        car_checkupMileage <~ tryField "MILEAGE",
        car_make <~ makeFld "MAKE",
        car_makeYear <~ tryField "VEHICLE_TYPE",
        car_model <~ modelFld "MAKE" "MODEL",
        car_plateNum <~ tryField "LICENCE_PLATE_NO",
        car_vin <~ field "VIN_NUMBER",
        car_warrantyEnd <~ tryField "VALID_TO",
        car_warrantyStart <~ tryField "VALID_FROM"]

-- | A common set for field mappings utilized by universal format.
universalFieldsCommon :: Dict [(String, Field Text)]
universalFieldsCommon = do
  (makeFld, modelFld) <- makeModelParser
  cols <- asks colorsDict
  return $
    [ car_buyDate <~ tryField "Дата покупки"
    , car_checkupDate <~ tryField "Дата последнего ТО"
    , car_color <~ tableField cols "Цвет"
    , car_make <~ makeFld "Марка"
    , car_makeYear <~ tryField "Год производства автомобиля"
    , car_model <~ modelFld "Марка" "Модель"
    , car_plateNum <~ tryField "Госномер"
    , cardNumber <~ tryField "Номер карты участника"
    , cardOwner <~ tryField "ФИО владельца карты"
    , manager <~ tryField "ФИО менеджера"
    , milageTO <~ tryField "Пробег при регистрации в программе"
    , ownerEmail <~ tryField "Email владельца"
    , ownerName <~ tryField "ФИО владельца"
    , ownerPhone <~ tryField "Контактный телефон владельца"
    , serviceInterval <~ tryField "Межсервисный интервал"
    , validFrom <~ tryField "Дата регистрации в программе"
    , validUntil <~ tryField "Программа действует до (дата)"
    , validUntilMilage <~ tryField "Программа действует до (пробег)"
    ]

universal :: Dict (String -> Model)
universal = do
    com <- universalFieldsCommon
    return $ \p -> model p $
                   com ++
                   [ car_vin <~ field "VIN"
                   ]

vtb24 :: Dict Model
vtb24 = model "vtb24" <$> universalFieldsCommon
