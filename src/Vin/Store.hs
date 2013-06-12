{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Vin.Store (
    VinUploadException(..),
    dbCreateVin, sinkXFile
    ) where

import Control.Arrow
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.Trans.Resource
import Control.Concurrent.MVar

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.CSV.Conduit hiding (Row, MapRow)

import Data.Char (isSpace)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.Typeable

import qualified System.IO as IO

import Carma.HTTP

import Vin.Field
import Vin.Model
import Vin.Load
import Vin.Utils

data VinUploadException = VinUploadException {
    vReason :: T.Text,
    vFilePath :: Maybe FilePath }
        deriving (Show, Typeable)

instance Exception VinUploadException

dbCreateVin :: ByteString
            -> ByteString
            -> Row 
            -> ResourceT CarmaIO ()
dbCreateVin owner programRef val
    | M.member "carVin" val = dbCreateRow
    | otherwise = return ()
        where
          dbCreateRow = do
            lift $ createInstance "contract" $
                        HM.insert "owner" owner $
                        HM.insert "program" programRef $
                        HM.fromList $
                        map (T.encodeUtf8 *** T.encodeUtf8) $
                        M.toList val
            return ()
        --     sets = mapM_ (\k -> redisSetWithKey' (key k) val) vins
        --     -- It seems that car can have several keys
        --     -- Duplicate as temporary solution
        --     vins = T.words $ val M.! car_vin
        --     key k = T.concat ["vin:", k]

parseRow :: Model -> DataRow -> (Maybe String, Row)
parseRow m r = case parse m $ decodeRow r of
    ([], s) -> (Nothing, s)
    (es, s) -> (Just $ showErrors es, s)

trimKeys :: DataRow -> DataRow
trimKeys = M.mapKeys trim where
    trim = fst . C8.spanEnd isSpace . snd . C8.span isSpace

sinkXFile
    :: MonadResource m
    => (Row -> m ())
    -> FilePath
    -> FilePath
    -> (Int -> Int -> m ())
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
    -> (Row -> m ())
    -> (Int -> Int -> m ())
    -> Conduit DataRowMaybeError m (DataRow, String)

storeCorrect _ store stats =
    let
        alloc = newMVar (0, 0)
        cleanup = const $ return ()
        updateStats m = liftIO (readMVar m) >>= uncurry stats
        newUpload m = liftIO (modifyMVar_ m (return . (succ *** succ))) >> updateStats m
        newFail m = liftIO (modifyMVar_ m (return . first succ)) >> updateStats m
    in
      bracketP alloc cleanup $
        \statsVar -> CL.mapMaybeM $
          \(src, r) -> case r of
            -- No errors, save store row
            Right (Nothing, dat) -> do
                store dat
                newUpload statsVar
                return Nothing
            -- Errors, store parsed row and produce errors
            Right (Just err, dat) -> do
                store dat
                newFail statsVar
                return $ Just (src, err)
            -- No parsed data, only error
            Left e' -> do
                newFail statsVar
                return $ Just (src, e')


storeErrorLog
    :: MonadResource m
    => FilePath
    -> Conduit (DataRow, String) m DataRow
storeErrorLog fLog = bracketP
    (do
        hFile <- IO.openFile fLog IO.WriteMode
        IO.hSetEncoding hFile IO.utf8
        return hFile)
    IO.hClose
    (\h -> CL.mapM (\(r, err) -> do
                      liftIO $ IO.hPutStrLn h err
                      return r))

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
        csvSettings = defCSVSettings { csvSep = ';' }
