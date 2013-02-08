{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Vin.Store (
    VinUploadException(..),
    redisSetVin, sinkXFile
    ) where

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.Conduit.Util hiding (zip)
import Data.CSV.Conduit hiding (Row, MapRow)

import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.Typeable

import Database.Redis as R

import qualified System.IO as IO

import Vin.Field
import Vin.Model
import Vin.Load
import Vin.Utils

data VinUploadException = VinUploadException {
    vReason :: T.Text,
    vFilePath :: Maybe FilePath }
        deriving (Show, Typeable)

instance Exception VinUploadException

redisSetVin :: R.Connection -> Row -> IO ()
redisSetVin c val
    | M.member "car_vin" val = runRedis c sets
    | otherwise = return ()
        where
            sets = mapM_ (\k -> redisSetWithKey' (key k) val) vins
            -- It seems that car can have several keys
            -- Duplicate as temporary solution
            vins = T.words $ val M.! "car_vin"
            key k = T.concat ["vin:", k]

redisSetWithKey' :: Text -> Row -> Redis ()
redisSetWithKey' key val = do
    res <- hmset (T.encodeUtf8 key) $ map (T.encodeUtf8 *** T.encodeUtf8) $ M.toList val
    case res of
        Left err -> liftIO $ print err -- ???
        _ -> return ()

parseRow :: Model -> DataRow -> (Maybe String, Row)
parseRow m r = case parse m $ decodeRow r of
    ([], s) -> (Nothing, s)
    (es, s) -> (Just $ showErrors es, s)

trimKeys :: DataRow -> DataRow
trimKeys = M.mapKeys trim where
    trim = fst . C8.spanEnd isSpace . snd . C8.span isSpace

sinkXFile
    :: MonadResource m
    => (R.Connection -> Row -> IO ())
    -> FilePath
    -> FilePath
    -> (Int -> Int -> IO ())
    -> Model
    -> Sink DataRowError m ()
sinkXFile store fError fLog stats ml
    =   safeMap trimKeys
    =$ safeMap (parseRow ml)
    =$ storeCorrect ml store stats
    =$ storeErrorLog fLog
    =$ writeIncorrect fError

storeCorrect
    :: MonadResource m
    => Model
    -> (R.Connection -> Row -> IO ())
    -> (Int -> Int -> IO ())
    -> Conduit DataRowMaybeError m (DataRow, String)

storeCorrect _ store stats = conduitIO
    ((,) <$> R.connect R.defaultConnectInfo <*> newMVar (0, 0))
    (\(conn, _) -> runRedis conn quit >> return ())
    (\(conn, statsVar) (src, r) -> case r of
        -- No errors, save store row
        Right (Nothing, dat) -> liftIO $ do
            store conn dat
            newUpload statsVar
            return (IOProducing [])
        -- Errors, store parsed row and produce errors
        Right (Just err, dat) -> liftIO $ do
            store conn dat
            newFail statsVar
            return (IOProducing [(src, err)])
        -- No parsed data, only error
        Left e' -> liftIO $ do
            newFail statsVar
            return $ IOProducing [(src, e')])
    (const $ return [])
    where
        updateStats m = readMVar m >>= uncurry stats
        newUpload m = modifyMVar_ m (return . (succ *** succ)) >> updateStats m
        newFail m = modifyMVar_ m (return . first succ) >> updateStats m

storeErrorLog
    :: MonadResource m
    => FilePath
    -> Conduit (DataRow, String) m DataRow
storeErrorLog fLog = conduitIO
    (do
        hFile <- IO.openFile fLog IO.WriteMode
        IO.hSetEncoding hFile IO.utf8
        return hFile)
    IO.hClose
    (\h (r, err) -> do
        liftIO $ IO.hPutStrLn h err
        return $ IOProducing [r])
    (const $ return [])

writeIncorrect :: MonadResource m => FilePath -> Sink DataRow m ()
writeIncorrect fp = do
    res <- CL.peek
    case res of
      Nothing -> return ()
      Just _  -> do
          fp' <- writeRows fp
          throw $ VinUploadException "Errors during load: " (Just fp')

writeRows :: MonadResource m => FilePath -> Sink DataRow m FilePath
writeRows fp
    =  (writeHeaders csvSettings >> fromCSV csvSettings)
    =$ sinkFile fp >> return fp
    where
        csvSettings = defCSVSettings { csvOutputColSep = ';' }
