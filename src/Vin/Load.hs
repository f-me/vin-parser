module Vin.Load (
    csv, xlsx,
    Loader, loadersContentType, loadersExtension
    ) where

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import Data.CSV.Conduit hiding (Row, MapRow)

import qualified Codec.Xlsx.Parser as Xlsx

import qualified Data.Text.Encoding as T

import Vin.Utils

-- | Load CSV
csv
    :: MonadResource m
    => FilePath
    -> Source m DataRow
csv f = sourceFile f $= intoCSV csvSettings $= CL.map decodeCP1251 where
    csvSettings = defCSVSettings { csvSep = ';', csvOutputColSep = ';' }

-- | Load XLSX
xlsx
    :: MonadResource m
    => FilePath
    -> IO (Source m DataRow)
xlsx f = do
    x <- Xlsx.xlsx f
    return (Xlsx.sheetSource x 0 Xlsx.convertToText Xlsx.convertToText $= CL.map encode)

-- FIXME: Trick with IO used for xlsx to preload file and then create source from it
-- I don't know the right way to do this
type Loader m = FilePath -> IO (Source m DataRow)

-- | Loaders by content type
loadersContentType :: MonadResource m => M.Map String (Loader m)
loadersContentType = M.fromList [
    ("text/csv", return . csv),
    ("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", xlsx)]

loadersExtension :: MonadResource m => M.Map String (Loader m)
loadersExtension = M.fromList [
    (".csv", return . csv),
    (".xlsx", xlsx)]

-- Old functions:

encode :: Xlsx.MapRow -> DataRow
encode m = M.map T.encodeUtf8 m' where
        m' = M.mapKeys T.encodeUtf8 m
