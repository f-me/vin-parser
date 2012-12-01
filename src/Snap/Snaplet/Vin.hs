{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Snap.Snaplet.Vin
    ( Vin
    , vinInit
    , initUploadState, uploadData, getState, removeAlert
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
import           Data.String
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
        alertInsert s $ infoAlert (T.pack f) "Uploading..." (T.decodeUtf8 partF)
    
    statsVar <- liftIO $ newMVar (0, 0)

    let
        uploadStats :: Int -> Int -> IO ()
        uploadStats total valid = do
            swapMVar statsVar (total, valid)
            alertUpdate s $ infoAlert (T.pack f) msg (T.decodeUtf8 partF)
            where
                msg = T.concat [
                    "Uploading... total rows processed: ",
                    T.pack . show $ total,
                    ", rows uploaded: ",
                    T.pack . show $ valid]
        
    liftIO $ forkIO $ do
        loadFile fUploaded fError fLog program (contentType . T.unpack . T.decodeUtf8 . partContentType $ info) uploadStats
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
                doneAlert = alertUpdate s $ (successAlert (T.pack f) resultMessage (T.decodeUtf8 partF)
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

initUploadState :: String -> Handler b Vin ()
initUploadState f = do
    s <- gets _alerts
    liftIO $ alertInsert s $ infoAlert (T.pack f) "Uploading..." (T.pack f)

uploadData :: String -> String -> Handler b Vin ()
uploadData program f = do
    s <- gets _alerts
    statsVar <- liftIO $ newMVar (0, 0)

    let
        uploadStats :: Int -> Int -> IO ()
        uploadStats total valid = do
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

    liftIO $ forkIO
        $ (loadFile fUploaded fError fLog (T.encodeUtf8 . T.pack $ program) (extension $ takeExtension fUploaded) uploadStats >> endWith Nothing)
        `E.catches` [
            E.Handler (\(VinUploadException r _) -> endWith (Just r)),
            E.Handler (\(ex :: E.SomeException) -> endWith (Just $ fromString $ show ex))]

    writeBS "Ok"
    where
        unquote "" = ""
        unquote s
            | head s == '"' = init . tail $ s
            | otherwise = s
        partFs = unquote f
        fError = "resources" </> "static" </> (partFs ++ ".error.csv")
        fLog = "resources" </> "static" </> (partFs ++ ".error.log")
        fErrorLink = "s/" ++ partFs ++ ".error.csv"
        fLogLink = "s/" ++ partFs ++ ".error.log"
        fUploaded = "resources" </> "static" </> "fileupload" </> "report" </> "upload" </> "data" </> f

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
    ("/upload", method POST upload),
    ("/state", method GET getState),
    ("/state", method POST removeAlert)]

vinInit :: SnapletInit b Vin
vinInit = makeSnaplet "vin" "Some description" Nothing $ do
    alerts' <- liftIO . newMVar $ Alerts M.empty
    -- addRoutes routes
    return $ Vin alerts'
