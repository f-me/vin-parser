{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Vin.Field (
    -- * Reading fields
    Row,
    decodeRow,
    FieldError(..),
    showError, showErrors,
    Field(..),
    fieldRead,
    FromField(..),
    field, tryField, with,
    evalField,

    -- * Verify and postprocess fields
    FieldType,
    text, time, timeAt, int, email, vin,

    -- * Converters
    posix, posixZoned, look,

    -- * Special field readers
    timeField,
    tableField,

    -- * Combine
    (<:~),
    ModelField,
    (<~)
    ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer

import Data.ByteString (ByteString)
import Data.Char (isSpace, toLower)
import Data.List (intercalate)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import Data.Time
import Data.Time.Clock.POSIX
import System.Locale
import Text.Email.Validate (isValid)
import Text.Read (readMaybe)

-- | Row is Map from field name to its value
type Row = M.Map Text Text

-- | Decode @ByteString@ to @Text@ and trim whitespaces in @Row@
decodeRow :: M.Map ByteString ByteString -> Row
decodeRow = M.mapKeys decode . M.map decode where
    decode :: ByteString -> Text
    decode = T.dropAround isSpace . T.decodeUtf8

-- | Field error is
-- * Field not found
-- * Error reading field with field name and error description
data FieldError = NoField String | FieldError String String
    deriving (Eq, Ord, Read, Show)

-- | Show @FieldError@
showError :: FieldError -> String
showError (NoField name) = "Отсутствует поле '" ++ name ++ "'"
showError (FieldError name what) = "Ошибка в поле '" ++ name ++ "': " ++ what

-- | Show list of errors
showErrors :: [FieldError] -> String
showErrors = intercalate ", " . map showError

-- | Field reader
-- It's Functor, Applicative, Alternative and Monad.
-- Prefer using Applicative if you want to collect field errors:
--
-- >(++) <$> field "one" <*> field "two"
--
-- Will lead to two NoField errors, while
--
-- >do { x <- field "one"; y <- field "two"; return (x ++ y) }
--
-- Will fail on first @field@ with only one error
--
newtype Field a = Field {
    runField :: ReaderT Row (Writer [FieldError]) (Maybe a) }

instance Functor Field where
    fmap f = Field . fmap (fmap f) . runField

instance Applicative Field where
    pure = Field . pure . pure
    f <*> x = Field $ do
        f' <- runField f
        x' <- runField x
        return $ f' <*> x'

instance Alternative Field where
    empty = Field $ return Nothing
    l <|> r = Field $ do
        l' <- runField l
        r' <- runField r
        return $ l' <|> r'

instance Monad Field where
    return = Field . return . Just
    x >>= f = Field $ do
        x' <- runField x
        maybe (return Nothing) (runField . f) x'

instance MonadReader Row Field where
    ask = Field $ fmap Just ask
    local f (Field act) = Field $ local f act
    reader f = Field $ fmap Just $ reader f

-- | Read field with @read@, trimming whitespaces
fieldRead :: Read a => String -> Text -> Either String a
fieldRead typeName s = maybe err Right $ readMaybe str where
    str = T.unpack s
    err = Left $ "Невозможно преобразовать '" ++ str ++ "' в " ++ typeName

-- | Fail with error
fieldError :: FieldError -> Field a
fieldError e = do
    Field $ lift $ fmap Just $ tell [e]
    empty

-- | Type, that can be read from field
class FromField a where
    fromField :: Text -> Either String a

instance FromField a => FromField (Maybe a) where
    fromField s
        | T.null s = Right Nothing
        | otherwise = fmap Just $ fromField s

-- | Read field from @Row@
field :: FromField a => String -> Field a
field name = do
    value <- asks (M.lookup (fromString name))
    maybe (fieldError (NoField name)) (either (fieldError . FieldError name) return . fromField) value

-- | Try read field ignoring @NoField@ error and treating empty field as no input
tryField :: FromField (Maybe a) => String -> Field a
tryField name = do
    value <- asks (M.lookup (fromString name))
    maybe empty (either (fieldError . FieldError name) (maybe empty return) . fromField) value

-- | Perform failable action on field
-- Accepts field reader (@field@ or @tryField@ or even custom ones, like @tableField@),
-- field name and function
-- The only need is to get field name to throw proper FieldError on fail
--
-- >with field "foo" (look tbl)
-- >with tryField "bar" verifySmth
--
with :: (String -> Field a) -> String -> (a -> Either String b) -> Field b
with fld name f  = fld name >>= either (fieldError . FieldError name) return . f

-- | Evaluate @Field@, collecting errors and result.
evalField :: Field a -> Row -> Either [FieldError] (Maybe a)
evalField act r = case runWriter $ runReaderT (runField act) r of
    (Just v, _) -> Right (Just v)
    (Nothing, []) -> Right Nothing
    (Nothing, errs) -> Left errs

-- | @FieldType@ used to convert field data to @Text@.
type FieldType a = a -> Text

instance FromField Text where
    fromField = Right

-- | Dublin Julian time used in xlsx
instance FromField POSIXTime where
    fromField = fmap toPOSIX . fieldRead "время" where
        toPOSIX :: Double -> POSIXTime
        toPOSIX = toPOSIX' . properFraction where
            toPOSIX' (d, tm) = utcTimeToPOSIXSeconds utct where
                day = ModifiedJulianDay $ d - 2 + 2415020 - 2400000
                utct = UTCTime day (fromInteger . floor $ tm * 60 * 60 * 24)

-- | Try to parse LocalTime
-- Use @posix@ or @posixZoned@ to convert it to POSIXTime
instance FromField LocalTime where
    fromField s = maybe err Right $ msum $ map parse fmts where
        str = T.unpack s
        err = Left $ "Невозможно преобразовать '" ++ str ++ "' в дату"
        parse :: String -> Maybe LocalTime
        parse fmt = parseTime defaultTimeLocale fmt str
        fmts :: [String]
        fmts = [
            "%d/%m/%Y",
            "%e/%m/%Y",
            "%e/%m/%Y %k:%M",
            "%d.%m.%Y",
            "%e %b %Y",
            "%e-%b-%Y",
            "%d-%b-%y",
            "%d-%m-%y",
            "%d-%m-%Y",
            "%b %Y",
            "%Y-%m-%dT%H:%M:%S",
            "%d.%m.%Y %H:%M:%S"]

-- | Parse POSIXTime or LocalTime
instance FromField Time where
    fromField s = fmap TimePOSIX (fromField s) <|> fmap TimeLocal (fromField s)

instance FromField Int where
    fromField = fieldRead "число"

instance FromField Email where
    fromField s
        | isValid (T.unpack s) = Right $ Email s
        | otherwise = Left $ "'" ++ (T.unpack s) ++ "' не соответствует формату e-mail"

instance FromField VIN where
    fromField s = case map convert $ T.unpack s of
            str
                | length str == 17 -> Right $ VIN $ T.pack str
                | otherwise -> Left $ "Неверный VIN номер '" ++ str ++ "', должен состоять из 17 символов"
        where
        convert :: Char -> Char
        convert ch = case ch of
            'А' -> 'A'
            'В' -> 'B'
            'Е' -> 'E'
            'З' -> '3'
            'К' -> 'K'
            'М' -> 'M'
            'О' -> 'O'
            'Р' -> 'P'
            'С' -> 'C'
            'Т' -> 'T'
            'У' -> 'Y'
            'Х' -> 'X'
            _ -> ch

-- | Text field type
text :: FieldType Text
text = id

-- | Time field
data Time = TimePOSIX POSIXTime | TimeLocal LocalTime

-- | Time field type at 'MSK'
time :: FieldType Time
time = timeAt "+0400"

-- | Time at time zone
timeAt :: String -> FieldType Time
timeAt tz tm = showTime $ case tm of
    TimePOSIX posixTm -> posixTm
    TimeLocal localTm -> posixZoned (read tz) localTm
    where
        showTime :: POSIXTime -> Text
        showTime = T.pack . show' . floor where
            show' :: Integer -> String
            show' = show

-- | Integer field
int :: FieldType Int
int = T.pack . show

-- | Email
newtype Email = Email { getEmail :: Text } deriving (Eq, Ord, Read, Show)

-- | Email field type
email :: FieldType Email
email = getEmail

newtype VIN = VIN { getVIN :: Text } deriving (Eq, Ord, Read, Show)

-- | VIN field
vin :: FieldType VIN
vin = getVIN

-- | Convert @LocalTime@ to @POSIXTime@, using 'MSK' time zone
posix :: LocalTime -> POSIXTime
posix = posixZoned (read "+0400")

-- | Convert @LocalTime@ to @POSIXTime@ at time zone specified
posixZoned :: TimeZone -> LocalTime -> POSIXTime
posixZoned tz = utcTimeToPOSIXSeconds . localTimeToUTC tz

-- | Look value in table
look :: M.Map Text Text -> Text -> Either String Text
look tbl value = maybe err Right $ M.lookup (lower value) tbl' where
    lower :: Text -> Text
    lower = T.map toLower
    tbl' :: M.Map Text Text
    tbl' = M.mapKeys lower tbl
    err :: Either String Text
    err = Left $ "Неверное значение: '" ++ T.unpack value ++ "'"

-- | Tries to read field as POSIXTime and LocalTime at 'MSK'
timeField :: String -> Field POSIXTime
timeField name = field name <|> (fmap posix $ field name)

-- | Lookup field value in @Map@
tableField :: M.Map Text Text -> String -> Field Text
tableField tbl name = with tryField name $ look tbl

-- | Combine @Field@ and @FieldType@, making function, which will read field,
-- producing possibly empty output or error.
(<:~) :: FieldType a -> Field a -> Field Text
ftype <:~ fld = fmap ftype fld

-- | Field name with @FieldType@
type ModelField a = (String, FieldType a)

-- | Combine @ModelField@ and @Field@
(<~) :: ModelField a -> Field a -> (String, Field Text)
(name, ftype) <~ fld = (name, ftype <:~ fld)
