{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Vin.Utils where

import           Control.Applicative
import           Control.Exception
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import           Data.Typeable

import           Control.Monad.IO.Class (liftIO)
import           Codec.Text.IConv
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.CSV.Conduit  hiding (MapRow, Row)
-- import           Data.Encoding (decodeStrictByteString, encodeStrictByteString)
-- import           Data.Encoding.CP1251
import           Database.Redis as R

import           Data.Xlsx.Parser

import Data.String

import Vin.Text

type Row = M.Map ByteString ByteString


data VinUploadException = VinUploadException
    { vReason   :: Text
    , vFilePath :: Maybe FilePath
    } deriving (Show, Typeable)


instance Exception VinUploadException

redisSetVin :: R.Connection -> Row -> IO ()
redisSetVin c val
  = runRedis c
  $ mapM_ (\k -> redisSetWithKey' (mkKey k) val) vins
  where
    -- It seems that car can have several VINs
    -- For now dublicate records
    vins = B.words $ val M.! "vin"
    mkKey k = B.concat ["vin:", k]


redisSetWithKey' :: ByteString -> Row -> Redis ()
redisSetWithKey' key val = do
  res <- hmset key $ M.toList val
  case res of
    Left err -> liftIO $ print err
    _ -> return ()


loadCsvFile
    :: (Connection -> Row -> IO ())
    -> FilePath
    -> FilePath
    -> TextModel
    -> IO ()
loadCsvFile store fInput fError textModel = runResourceT
    $  sourceFile fInput
    $= intoCSV csvSettings
    $= CL.map decodeCP1251
    $$ sinkXFile store fError textModel
    where
        csvSettings = defCSVSettings { csvSep = ';' }

loadXlsxFile
    :: (Connection -> Row -> IO ())
    -> FilePath
    -> FilePath
    -> TextModel
    -> IO ()
loadXlsxFile store fInput fError textModel = do
    x <- xlsx fInput
    runResourceT
        $  sheetRows x 0
        $= CL.map encode
        $$ sinkXFile store fError textModel

parseRow
    :: TextModel
    -> Row
    -> Either (Row, [RowError ByteString TypeError]) Row
parseRow m r = case parse m r of
    Left e -> Left (r, e)
    Right s -> Right $ M.fromList $ zip (map (fromString . fst) (modelFields m)) s

sinkXFile
    :: MonadResource m
    => (Connection -> Row -> IO ())
    -> FilePath
    -> TextModel
    -> Sink Row m ()
sinkXFile store fError textModel
    =  CL.map (parseRow textModel)
    =$ storeCorrect textModel store
    =$ CL.map (encodeCP1251 . fst)
    =$ writeIncorrect fError

storeCorrect
    :: MonadResource m
    => TextModel
    -> (R.Connection -> Row -> IO ())
    -> Conduit (Either a Row) m a
storeCorrect textModel store = conduitIO
    (R.connect R.defaultConnectInfo)
    (\conn -> runRedis conn quit >> return ())
    (\conn row -> case row of
        Right r -> do
            liftIO $ store conn r
            return $ IOProducing []
        Left r -> return $ IOProducing [r])
    (const $ return [])


writeIncorrect :: MonadResource m => FilePath -> Sink Row m ()
writeIncorrect fp = do
    res <- CL.peek
    case res of
      Nothing -> return ()
      Just _  -> do
          fp' <- writeRows fp
          throw $ VinUploadException "Errors during load: " (Just fp')


writeRows :: MonadResource m => FilePath -> Sink Row m FilePath
writeRows fp
    =  fromCSV csvSettings
    =$ sinkFile fp >> return fp
  where
    csvSettings = defCSVSettings { csvOutputColSep = ';' }

encode :: MapRow -> Row
encode m = M.map T.encodeUtf8 m'
  where
    m' = M.mapKeys T.encodeUtf8 m

encodeCP1251 :: Row -> Row
encodeCP1251 m = M.map enc m'
  where
    m' = M.mapKeys enc m
    enc s = B.concat . L.toChunks . convert "UTF-8" "CP1251" $ L.fromChunks [s]


decodeCP1251 :: Row -> Row
decodeCP1251 m = M.map enc m'
  where
    m' = M.mapKeys enc m
    enc s = B.concat . L.toChunks . convert "CP1251" "UTF-8" $ L.fromChunks [s]
