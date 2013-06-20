{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Snap.Snaplet.Vin
    ( Vin
    , vinInit
    , initUploadState, uploadData, getState, removeAlert
    ) where

import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import           Data.Map as M
import           Data.Maybe
import           GHC.Generics

import           Control.Monad.State
import           Data.Aeson
import           Data.Lens.Template
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           Snap.Http.Server.Config as S
import           Snap.Snaplet
import           System.FilePath

import           Carma.HTTP (CarmaIO)

import           Vin.Import

data Alert a = Alert {
    alertId :: a,
    alertType :: Text,
    alertMessage :: Text,
    alertVinFile :: Text,
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
    _alerts :: MVar (Alerts Text) }

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
alert :: Text -> a -> Text -> Text -> Alert a
alert t i msg vinFile = Alert i t msg vinFile Nothing Nothing

-- | Info alert
infoAlert :: a -> Text -> Text -> Alert a
infoAlert = alert "info"

-- | Success alert
successAlert :: a -> Text -> Text -> Alert a
successAlert = alert "success"

-- | Error alert
errorAlert :: a -> Text -> Text -> Alert a
errorAlert = alert "error"

-- | Add error file to alert
withErrorFile :: Alert a -> FilePath -> Alert a
withErrorFile a f = a { alertErrorFile = Just f }

-- | Add error log file to alert
withErrorLogFile :: Alert a -> FilePath -> Alert a
withErrorLogFile a f = a { alertErrorLogFile = Just f }

-- | Obtain port number of the Snap application.
currentPort :: IO Int
currentPort = do
  sCfg <- commandLineConfig (emptyConfig :: S.Config Snap a)
  return $ case getPort sCfg of
             Just n -> n
             Nothing -> error "No port"

initUploadState :: String -> Handler b Vin ()
initUploadState f = do
    s <- gets _alerts
    liftIO $ alertInsert s $ infoAlert (T.pack f) "Uploading..." (T.pack f)

uploadData :: ByteString
           -- ^ Owner field value.
           -> ByteString
           -- ^ Parent program reference (@program:12@).
           -> String
           -- ^ Vin format name.
           -> FilePath
           -- ^ Path to a VIN data file.
           -> Handler b Vin ()
uploadData owner program vinFormat fUploaded = do
    s <- gets _alerts
    statsVar <- liftIO $ newMVar (0, 0)

    let
        uploadStats :: Int -> Int -> ResourceT CarmaIO ()
        uploadStats total valid = liftIO $ do
            swapMVar statsVar (total, valid)
            alertUpdate s $ infoAlert (T.pack f) msg (T.pack f)
            where
                msg = T.concat [
                    "Uploading... total rows processed: ",
                    T.pack $ show total,
                    ", rows uploaded: ",
                    T.pack $ show valid]

        endWith :: Maybe T.Text -> IO ()
        endWith result = do
            (total, valid) <- readMVar statsVar
            let
                statsMsg = T.concat [
                    "Total rows processed: ",
                    T.pack $ show total,
                    ", rows uploaded: ",
                    T.pack $ show valid]
                resultMsg = T.concat [
                    maybe "Done. " (\err -> T.concat ["Failed with: ", err, ". "]) result,
                    statsMsg]
            alertUpdate s $ (if isJust result then errorAlert else successAlert) (T.pack f) resultMsg (T.pack f)
                `withErrorFile` fErrorLink
                `withErrorLogFile` fLogLink

    carmaPort <- liftIO currentPort
    liftIO $ forkIO $
        (loadFile fUploaded fError fLog owner program vinFormat (extension $ takeExtension fUploaded) uploadStats carmaPort >> endWith Nothing)
        `E.catches` [
            E.Handler (\(ex :: VinUploadException) -> endWith Nothing),
            E.Handler (\(ex :: E.SomeException) -> endWith (Just $ fromString $ show ex))]

    writeBS "Ok"
    where
        unquote "" = ""
        unquote s
            | head s == '"' = init . tail $ s
            | otherwise = s
        f = takeFileName fUploaded
        partFs = unquote f
        fError = "resources" </> "static" </> (partFs ++ ".error.csv")
        fLog = "resources" </> "static" </> (partFs ++ ".error.log")
        fErrorLink = "s/" ++ partFs ++ ".error.csv"
        fLogLink = "s/" ++ partFs ++ ".error.log"

getState :: Handler b Vin ()
getState = do
    s <- gets _alerts
    alerts' <- liftIO $ readMVar s
    writeLBS $ encode alerts'

removeAlert :: Handler b Vin ()
removeAlert = do
    s <- gets _alerts
    res <- getParam "id"
    maybe (return ()) (liftIO . alertDelete s . T.decodeUtf8) res

routes :: [(ByteString, Handler b Vin ())]
routes = [
    ("/state", method GET getState),
    ("/state", method POST removeAlert)]

vinInit :: SnapletInit b Vin
vinInit = makeSnaplet "vin" "Some description" Nothing $ do
    alerts' <- liftIO . newMVar $ Alerts M.empty
    -- addRoutes routes
    return $ Vin alerts'
