module Vin.Import (
    VinUploadException(..),
    ContentType,
    contentType, extension,
    importData,
    loadFile
    ) where

import Control.Exception (throw)
import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Map as M

import qualified Data.Text as T (pack)

import Carma.HTTP

import Vin.Model
import Vin.Models (models, runDict)
import Vin.Load
import Vin.Store

-- | Try to get maybe value
try :: Maybe a -> String -> IO a
try Nothing msg = throw $ VinUploadException (T.pack msg) Nothing
try (Just x) _ = return x

-- | Content type
type ContentType = Either String String

-- | By ContentType
contentType :: String -> ContentType
contentType = Left

-- | By file extension
extension :: String -> ContentType
extension = Right

-- | Import data
-- TODO: Move to import
importData
    :: (String -> Maybe Model)
    -- ^ Lookup program by its name.
    -> FilePath
    -- ^ Input file
    -> FilePath
    -- ^ Errors file
    -> FilePath
    -- ^ Log file
    -> ByteString
    -- ^ Owner field value
    -> ByteString
    -- ^ Program name
    -> String
    -- ^ VIN file format name.
    -> ContentType
    -- ^ Content type
    -> (Int -> Int -> ResourceT CarmaIO ())
    -- ^ Progress
    -> Int
    -- ^ CaRMa port.
    -> IO ()

importData ms from failed errors owner program vinFormat content stats cp = do
    loader <- liftIO $ try (either (`M.lookup` loadersContentType) (`M.lookup` loadersExtension) content) "Unknown loader"
    -- loader <- try (M.lookup content ls) $ "Unknown loader"
    m <- liftIO $ try (ms vinFormat) $ "Unknown program"
    l <- liftIO $ loader from
    runCarma defaultCarmaOptions{carmaPort = cp} $ runResourceT $ (l $$ sinkXFile (dbCreateVin owner program) failed errors stats m)

loadFile :: FilePath -> FilePath -> FilePath -> ByteString -> ByteString -> String -> ContentType -> (Int -> Int -> ResourceT CarmaIO ()) -> Int -> IO ()
loadFile iFile eFile lFile owner pName vinFormat cType stats cp = do
    models' <- runDict models
    models'' <- try models' $ "Unable to load models"
    importData models'' iFile eFile lFile owner pName vinFormat cType stats cp
