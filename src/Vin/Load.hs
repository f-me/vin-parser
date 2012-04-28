module Vin.Load (
    csv, xlsx, loaders,
    importModel
    ) where

import Control.Monad.IO.Class (liftIO)

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.List
import qualified Data.Map as M
import Data.CSV.Conduit hiding (Row, MapRow)

import Vin.Models
import Vin.Utils

-- | Load CSV
csv
    :: MonadResource m
    => FilePath
    -> Source m Row
csv f = sourceFile f $= intoCSV csvSettings $= CL.map decodeCP1251 where
    csvSettings = defCSVSettings { csvOutputColSep = ';' }

-- | Load XLSX
xlsx
    :: MonadResource m
    => FilePath
    -> Source m Row
xlsx f = undefined

-- | Loaders by content type
loaders :: MonadResource m => M.Map String (FilePath -> Source m Row)
loaders = M.fromList [
    ("text/csv", csv),
    ("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", xlsx)]

-- | Try to get maybe value
try :: Maybe a -> String -> IO a
try Nothing msg = undefined -- throw $ VinUploadException blabla
try (Just x) _ = return x

-- | Import data
-- TODO: Move to import
importModel
    :: [TextModel]
    -- ^ Models
    -> M.Map String (FilePath -> Source (ResourceT IO) Row)
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

importModel ms ls from failed program content = do
    loader <- try (M.lookup content ls) $ "Unknown loader"
    m <- try (find ((== program) . modelProgram) ms) $ "Unknown program"
    runResourceT $ (loader from $$ sinkXFile redisSetVin failed m)
