module Vin.Import (
    VinUploadException(..),
    importData,
    loadFile
    ) where

import Control.Exception (throw)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Conduit
import Data.List (find)
import qualified Data.Map as M

import qualified Data.Text as T (pack)

import Vin.Model
import Vin.Models (models, runDict)
import Vin.Load
import Vin.Store

-- | Try to get maybe value
try :: Maybe a -> String -> IO a
try Nothing msg = throw $ VinUploadException (T.pack msg) Nothing
try (Just x) _ = return x

-- | Import data
-- TODO: Move to import
importData
    :: [Model]
    -- ^ Models
    -> M.Map String (Loader (ResourceT IO))
    -- ^ Loaders
    -> FilePath
    -- ^ Input file
    -> FilePath
    -- ^ Errors file
    -> FilePath
    -- ^ Log file
    -> String
    -- ^ Program name
    -> String
    -- ^ Content type
    -> (Int -> Int -> IO ())
    -- ^ Progress
    -> IO ()

importData ms ls from failed errors program content stats = do
    loader <- try (M.lookup content ls) $ "Unknown loader"
    m <- try (find ((== program) . modelProgram) ms) $ "Unknown program"
    l <- loader from
    runResourceT $ (l $$ sinkXFile redisSetVin failed errors stats m)

loadFile :: FilePath -> FilePath -> FilePath -> ByteString -> ByteString -> (Int -> Int -> IO ()) -> IO ()
loadFile iFile eFile lFile pName cType stats = do
    models' <- runDict models
    models'' <- try models' $ "Unable to load models"
    importData models'' loaders iFile eFile lFile (C8.unpack pName) (C8.unpack cType) stats
