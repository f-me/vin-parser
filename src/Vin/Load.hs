module Vin.Load (
    DataError,
    DataRowError, DataRowMaybeError,
    csv, xlsx,
    Loader, loadersContentType, loadersExtension,
    dup, safeMap, safeMapM
    ) where

import Control.Exception
import Control.DeepSeq
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import Data.CSV.Conduit hiding (Row, MapRow)

import qualified Codec.Xlsx.Parser as Xlsx

import qualified Data.Text.Encoding as T
import Data.Text.ICU.Convert as ICU

import Vin.Field (Row)
import Vin.Utils

type DataError c a = (c, Either String a)

type DataRowError = DataError DataRow DataRow

-- | Parsed DataRow with parse errors
type DataRowMaybeError = DataError DataRow (Maybe String, Row)

-- | Load CSV
csv
    :: MonadResource m
    => FilePath
    -> Source m DataRowError
csv f = do
  let csvSettings = defCSVSettings { csvSep = ';' }
  conv <- liftIO $ ICU.open "cp1251" Nothing
  sourceFile f $= intoCSV csvSettings $= CL.map dup $= safeMap (decodeICU conv) where
  

-- | Load XLSX
xlsx
    :: MonadResource m
    => FilePath
    -> IO (Source m DataRowError)
xlsx f = do
    x <- Xlsx.xlsx f
    return (Xlsx.sheetSource x 0 Xlsx.convertToText Xlsx.convertToText $= CL.map (dup . encode))

-- FIXME: Trick with IO used for xlsx to preload file and then create source from it
-- I don't know the right way to do this
type Loader m = FilePath -> IO (Source m DataRowError)

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

dup :: a -> DataError a a
dup r = (r, Right r)

-- | Wrap pure function within IO to catch exceptions and provide source data on error
safeMap :: (MonadResource m, NFData b) => (a -> b) -> Conduit (DataError c a) m (DataError c b)
safeMap f = safeMapM (Right . f)

safeMapM :: (MonadResource m, NFData b) => (a -> Either String b) -> Conduit (DataError c a) m (DataError c b)
safeMapM f = CL.mapM
    (\(r, i) -> case i of
        Left x -> return (r, Left x)
        Right v -> liftIO $ catch (produce f r v) (onError r))
    where
        produce :: NFData b => (a -> Either String b) -> c -> a -> IO (DataError c b)
        produce f' r v = do
            v' <- evaluate $ force $ f' v
            return (r, v')
        onError :: c -> SomeException -> IO (DataError c a)
        onError r e = return $ (r, Left $ show e)
