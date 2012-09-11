{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Snap.Snaplet.Vin
    ( Vin
    , vinInit
    ) where

import           Prelude hiding (catch)
import           Control.Applicative
import           Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Map (Map)
import           Data.Map as M
import           Data.Maybe
import           GHC.Generics

import           Control.Monad.CatchIO (finally, catch)
import           Control.Monad.State
import           Data.Aeson
import           Data.Lens.Template
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Util.FileUploads
import           System.FilePath
import           System.Directory
import           System.Posix.Files (createLink, removeLink)

import           Vin.Import

data Alert a = Alert {
    alertId :: a,
    alertType :: Text,
    alertMessage :: Text,
    alertVinFile :: ByteString,
    alertErrorFile :: Maybe FilePath,
    alertErrorLogFile :: Maybe FilePath  }
        deriving (Eq, Ord, Read, Show, Generic)

instance ToJSON a => ToJSON (Alert a)

newtype Alerts a = Alerts (Map a (Alert a))

instance ToJSON a => ToJSON (Alerts a) where
    toJSON (Alerts m) = object [("alerts", alerts')] where
        alerts' = toJSON $ M.elems m

insertAlert :: Ord a => Alert a -> Alerts a -> Alerts a
insertAlert a (Alerts m) = Alerts $ M.insert (alertId a) a m

updateAlert :: Ord a => Alert a -> Alerts a -> Alerts a
updateAlert a (Alerts m) = Alerts $ M.update (const $ Just a) (alertId a) m

deleteAlert :: Ord a => a -> Alerts a -> Alerts a
deleteAlert k (Alerts m) = Alerts $ M.delete k m

------------------------------------------------------------------------------
data Vin = Vin {
    _alerts :: MVar (Alerts ByteString) }

makeLens ''Vin

withAlerts :: MVar a -> (a -> a) -> IO ()
withAlerts m f = modifyMVar_ m (return . f)

alertInsert :: Ord a => MVar (Alerts a) -> Alert a -> IO ()
alertInsert mvar a = withAlerts mvar (insertAlert a)

alertUpdate :: Ord a => MVar (Alerts a) -> Alert a -> IO ()
alertUpdate mvar a = withAlerts mvar (updateAlert a)

alertDelete :: Ord a => MVar (Alerts a) -> a -> IO ()
alertDelete mvar k = withAlerts mvar (deleteAlert k)

-- | Generic alert
alert :: Text -> a -> Text -> ByteString -> Alert a
alert t i msg vinFile = Alert i t msg vinFile Nothing Nothing

-- | Info alert
infoAlert :: a -> Text -> ByteString -> Alert a
infoAlert = alert "info"

-- | Success alert
successAlert :: a -> Text -> ByteString -> Alert a
successAlert = alert "success"

-- | Error alert
errorAlert :: a -> Text -> ByteString -> Alert a
errorAlert = alert "error"

-- | Add error file to alert
withErrorFile :: Alert a -> FilePath -> Alert a
withErrorFile a f = a { alertErrorFile = Just f }

-- | Add error log file to alert
withErrorLogFile :: Alert a -> FilePath -> Alert a
withErrorLogFile a f = a { alertErrorLogFile = Just f }

------------------------------------------------------------------------------
-- | Upload file with VIN numbers.
upload :: Handler b Vin ()
upload = ifTop $ do
    d <- liftIO $ getTemporaryDirectory
    handleFileUploads d defaultUploadPolicy partUploadPolicy handler
        `catch` (writeText . fileUploadExceptionReason)
    where
        partUploadPolicy _ = allowWithMaximumSize $ 2 ^ (30 :: Int)
        
        handler [] = writeBS "no files"
        handler ((info, f):_) = do
            program <- fromMaybe "" <$> getParam "program"
            either
                (writeText . policyViolationExceptionReason)
                (action program info)
                f

action :: ByteString -> PartInfo -> String -> Handler b Vin ()
action program info f = do
    --liftIO $ createLink f f'
    liftIO $ copyFile f fUploaded
    s <- gets _alerts
    liftIO $ do
        alertInsert s $ infoAlert (B.pack f) "Uploading..." partF
    
    statsVar <- liftIO $ newMVar (0, 0)

    let
        uploadStats :: Int -> Int -> IO ()
        uploadStats total valid = do
            swapMVar statsVar (total, valid)
            alertUpdate s $ infoAlert (B.pack f) msg partF
            where
                msg = T.pack $ concat [
                    "Uploading... total rows processed: ",
                    show total,
                    ", rows uploaded: ",
                    show valid]
        
    liftIO $ forkIO $ do
        loadFile fUploaded fError fLog program (partContentType info) uploadStats
        `E.catches` [
            E.Handler (\(ex :: VinUploadException) -> return ()),
            E.Handler (\(ex :: E.SomeException) -> return ())]
        `finally` (do
            -- removeLink f'
            (total, valid) <- readMVar statsVar
            let
                resultMessage = T.pack $ concat [
                    "Done. Total rows processed: ",
                    show total,
                    ", rows uploaded: ",
                    show valid]
                doneAlert = alertUpdate s $ (successAlert (B.pack f) resultMessage partF
                    `withErrorFile` fErrorLink
                    `withErrorLogFile` fLogLink)
            doneAlert)
    
    writeBS "Ok"
    where
        partF = fromMaybe "" $ partFileName info
        unquote "" = ""
        unquote s
            | head s == '"' = init . tail $ s
            | otherwise = s
        partFs = unquote $ T.unpack $ T.decodeUtf8 partF
        fError = "resources/static/" ++ partFs ++ ".error.csv"
        fLog = "resources/static/" ++ partFs ++ ".error.log"
        fErrorLink = "s/" ++ partFs ++ ".error.csv"
        fLogLink = "s/" ++ partFs ++ ".error.log"
        fUploaded = "resources" </> "static" </> addExtension "uploaded" (takeExtension . T.unpack . T.decodeUtf8 $ partF)
        f' = f ++ "-link"

getState :: Handler b Vin ()
getState = do
    s <- gets _alerts
    alerts' <- liftIO $ readMVar s
    writeLBS $ encode alerts'

removeAlert :: Handler b Vin ()
removeAlert = do
    s <- gets _alerts
    res <- getParam "id"
    maybe (return ()) (liftIO . alertDelete s) res

routes :: [(ByteString, Handler b Vin ())]
routes = [
    ("/upload", method POST upload),
    ("/state", method GET getState),
    ("/state", method POST removeAlert)]

vinInit :: SnapletInit b Vin
vinInit = makeSnaplet "vin" "Some description" Nothing $ do
    alerts' <- liftIO . newMVar $ Alerts M.empty
    addRoutes routes
    return $ Vin alerts'
