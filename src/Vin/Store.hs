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
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Text as T (Text)
import Data.Typeable

import Database.Redis as R

import qualified System.IO as IO

import Vin.Text (TypeError(..))
import Vin.Text.String
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
    Right s -> Right $ M.fromList $ zip (map fst (modelFields m)) s

trimKeys :: DataRow -> DataRow
trimKeys = M.mapKeys trim where
    trim = fst . C8.spanEnd isSpace . snd . C8.span isSpace

sinkXFile
    :: MonadResource m
    => (R.Connection -> DataRow -> IO ())
    -> FilePath
    -> FilePath
    -> (Int -> Int -> IO ())
    -> Model
    -> Sink DataRow m ()
sinkXFile store fError fLog stats ml
    =   CL.map trimKeys
    =$ CL.map (parseRow ml)
    =$ storeCorrect ml store stats
    =$ storeErrorLog fLog
    =$ CL.map encodeCP1251
    =$ writeIncorrect fError

storeCorrect
    :: MonadResource m
    => Model
    -> (R.Connection -> DataRow -> IO ())
    -> (Int -> Int -> IO ())
    -> Conduit (Either a DataRow) m a

storeCorrect _ store stats = conduitIO
    ((,) <$> R.connect R.defaultConnectInfo <*> newMVar (0, 0))
    (\(conn, _) -> runRedis conn quit >> return ())
    (\(conn, statsVar) r -> case r of
        Right r' -> do
            liftIO $ do
                store conn r'
                newUpload statsVar
            return $ IOProducing []
        Left r' -> do
            liftIO $ newFail statsVar
            return $ IOProducing [r'])
    (const $ return [])
    where
        updateStats m = readMVar m >>= uncurry stats
        newUpload m = modifyMVar_ m (return . (succ *** succ)) >> updateStats m
        newFail m = modifyMVar_ m (return . first succ) >> updateStats m

storeErrorLog
    :: MonadResource m
    => FilePath
    -> Conduit ParseError m DataRow
storeErrorLog fLog = conduitIO
    (IO.openFile fLog IO.WriteMode)
    IO.hClose
    (\h (r, ers) -> do
        liftIO $ IO.hPutStrLn h $ formatErrors ers
        return $ IOProducing [r])
    (const $ return [])
    where
        formatErrors ers = intercalate "; " $ map formatError ers
        formatError (NoColumn name) = "No field " ++ decodeString name
        formatError (FieldError name (InvalidType msg)) =
            "Invalid field " ++ decodeString name ++ ": " ++ msg

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
