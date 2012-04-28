{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Vin.Store (
    VinUploadException(..),
    redisSetVin, sinkXFile
    ) where

import Control.Exception
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.CSV.Conduit hiding (Row, MapRow)

import qualified Data.Map as M
import Data.Text as T (Text)
import Data.Typeable

import Database.Redis as R

import Data.String

import Vin.Text (TypeError)
import Vin.Row
import Vin.Model
import Vin.Utils

data VinUploadException = VinUploadException {
    vReason :: T.Text,
    vFilePath :: Maybe FilePath }
        deriving (Show, Typeable)

instance Exception VinUploadException

redisSetVin :: R.Connection -> DataRow -> IO ()
redisSetVin c val = runRedis c sets where
    sets = mapM_ (\k -> redisSetWithKey' (key k) val) vins
    -- It seems that car can have several keys
    -- Duplicate as temporary solution
    vins = C8.words $ val M.! "vin"
    key k = C8.concat ["vin:", k]

redisSetWithKey' :: ByteString -> DataRow -> Redis ()
redisSetWithKey' key val = do
    res <- hmset key $ M.toList val
    case res of
        Left err -> liftIO $ print err -- ???
        _ -> return ()

type ParseError = (DataRow, [RowError ByteString TypeError])

parseRow :: Model -> DataRow -> Either ParseError DataRow
parseRow m r = case parse m r of
    Left e -> Left (r, e)
    Right s -> Right $ M.fromList $ zip (map (fromString . fst) (modelFields m)) s

sinkXFile
    :: MonadResource m
    => (R.Connection -> DataRow -> IO ())
    -> FilePath
    -> Model
    -> Sink DataRow m ()
sinkXFile store fError ml
    =  CL.map (parseRow ml)
    =$ storeCorrect ml store
    =$ CL.map (encodeCP1251 . fst)
    =$ writeIncorrect fError
storeCorrect
    :: MonadResource m
    => Model
    -> (R.Connection -> DataRow -> IO ())
    -> Conduit (Either a DataRow) m a

storeCorrect _ store = conduitIO
    (R.connect R.defaultConnectInfo)
    (\conn -> runRedis conn quit >> return ())
    (\conn r -> case r of
        Right r' -> do
            liftIO $ store conn r'
            return $ IOProducing []
        Left r' -> return $ IOProducing [r'])
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
    =  fromCSV csvSettings
    =$ sinkFile fp >> return fp
    where
        csvSettings = defCSVSettings { csvOutputColSep = ';' }
