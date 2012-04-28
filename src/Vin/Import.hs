module Vin.Import (
    VinUploadException(..),
    importData,
    loadFile
    ) where

import Data.ByteString (ByteString)
import Data.Conduit
import Data.List (find)
import qualified Data.Map as M

import Vin.Model
import Vin.Load
import Vin.Store

-- | Try to get maybe value
try :: Maybe a -> String -> IO a
try Nothing msg = undefined -- throw $ VinUploadException blabla
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
    -> String
    -- ^ Program name
    -> String
    -- ^ Content type
    -> IO ()

importData ms ls from failed program content = do
    loader <- try (M.lookup content ls) $ "Unknown loader"
    m <- try (find ((== program) . modelProgram) ms) $ "Unknown program"
    runResourceT $ (loader from $$ sinkXFile redisSetVin failed m)

loadFile :: FilePath -> FilePath -> ByteString -> ByteString -> IO ()
loadFile = undefined
