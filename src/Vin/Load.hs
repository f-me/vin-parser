module Vin.Load (
    csv, xlsx,
    Loader, loaders
    ) where

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import Data.CSV.Conduit hiding (Row, MapRow)

import qualified Data.Xlsx.Parser as Xlsx

import qualified Data.Text.Encoding as T

import Vin.Utils

-- | Load CSV
csv
    :: MonadResource m
    => FilePath
    -> Source m DataRow
csv f = sourceFile f $= intoCSV csvSettings $= CL.map decodeCP1251 where
    csvSettings = defCSVSettings { csvOutputColSep = ';' }

-- | Load XLSX
xlsx
    :: MonadResource m
    => FilePath
    -> Source m DataRow
xlsx = undefined

type Loader m = FilePath -> Source m DataRow

-- | Loaders by content type
loaders :: MonadResource m => M.Map String (Loader m)
loaders = M.fromList [
    ("text/csv", csv),
    ("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", xlsx)]

-- Old functions:

encode :: Xlsx.MapRow -> DataRow
encode m = M.map T.encodeUtf8 m' where
        m' = M.mapKeys T.encodeUtf8 m

{-
loadXlsxFile
    :: (Connection -> DataRow -> IO ())
    -> FilePath
    -> FilePath
    -> TextModel
    -> IO ()
loadXlsxFile store fInput fError textModel = do
    x <- Xlsx.xlsx fInput
    runResourceT
        $  Xlsx.sheetRows x 0
        $= CL.map encode
        $$ sinkXFile store fError textModel
-}
