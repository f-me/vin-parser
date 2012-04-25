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
import qualified Control.Exception as E
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Map (Map)
import           Data.Map as M
import           Data.Maybe
import           GHC.Generics
import           System.Directory (getTemporaryDirectory)

import           Control.Monad.CatchIO (finally, catch)
import           Control.Monad.State
import           Data.Aeson
import           Data.Lens.Template
import           Data.Text (Text)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Util.FileUploads
import           System.Posix.Files (createLink, removeLink)

import           Vin.Import


data Alert a = Alert
    { alertId        :: a
    , alertType      :: Text
    , alertMessage   :: Text
    , alertVinFile   :: ByteString
    , alertErrorFile :: Maybe FilePath
    } deriving (Show, Eq, Generic)


instance ToJSON a => ToJSON (Alert a)


newtype Alerts a = Alerts (Map a (Alert a))


instance ToJSON a => ToJSON (Alerts a) where
    toJSON (Alerts m) = object [("alerts", as)]
        where
          as = toJSON $ M.foldrWithKey f [] m
          f k v res = v { alertId = k } : res


insertAlert :: Ord a => Alert a -> Alerts a -> Alerts a
insertAlert a (Alerts m) =
    Alerts $ M.insert (alertId a) a m



updateAlert :: Ord a => Alert a -> Alerts a -> Alerts a
updateAlert a (Alerts m) =
    Alerts $ M.update (const $ Just a) (alertId a) m


deleteAlert :: Ord a => a -> Alerts a -> Alerts a
deleteAlert k (Alerts m) =
    Alerts $ M.delete k m


------------------------------------------------------------------------------
data Vin = Vin
    { _alerts :: MVar (Alerts ByteString)
    }


makeLens ''Vin


modifyMVar' :: MVar a -> (a -> a) -> IO ()
modifyMVar' m f =
    modifyMVar_ m (return . f)


------------------------------------------------------------------------------
-- | Upload file with VIN numbers.
upload :: Handler b Vin ()
upload = ifTop $ do
    d <- liftIO $ getTemporaryDirectory

    handleFileUploads d defaultUploadPolicy partUploadPolicy handler
      `catch` (writeText . fileUploadExceptionReason)

  where
    partUploadPolicy _ = allowWithMaximumSize $ 100 * 2^(20::Int)

    handler []        = writeBS "no files"
    handler ((info,f):_) = do
        program <- fromMaybe "" <$> getParam "program"

        either (writeText . policyViolationExceptionReason)
               (action program info)
               f


action :: ByteString -> PartInfo -> String -> Handler b Vin ()
action program info f = do
    liftIO $ createLink f f'

    s <- gets _alerts

    let a = Alert (B.pack f)
                  "info"
                  "обработка..."
                  (fromMaybe "" $ partFileName info)
                  Nothing

    liftIO $ modifyMVar' s $ insertAlert a

    liftIO $ forkIO $ do
        loadFile f' fError program $ partContentType info

        modifyMVar' s $ updateAlert a { alertType = "success"
                                     , alertMessage = "закончено"
                                     }

        `E.catches` [ E.Handler (\ (ex :: VinUploadException) ->
                                     modifyMVar' s $ updateAlert a { alertType = "error"
                                                                   , alertMessage = vReason ex
                                                                   , alertErrorFile = vFilePath ex }
                                )
                    , E.Handler (\ (ex :: E.SomeException) ->
                                     modifyMVar' s $ updateAlert a { alertType = "error"
                                                                   , alertMessage = T.pack $ show ex }
                                )
                    ]

        `finally` removeLink f'

    writeBS "Ok"
  where
    fError = "resources/static/error.csv"
    f' = f ++ "-link"


getState :: Handler b Vin ()
getState = do
    s <- gets _alerts
    as <- liftIO $ readMVar s
    writeLBS $ encode as


removeAlert :: Handler b Vin ()
removeAlert = do
    s <- gets _alerts

    res <- getParam "id"

    liftIO $ case res of
               Nothing -> return ()
               Just k  -> f s k

  where
    f s k = modifyMVar_ s $ \as ->
                  return $ deleteAlert k as


routes :: [(ByteString, Handler b Vin ())]
routes = [ ("/upload", method POST upload)
         , ("/state", method GET getState)
         , ("/state", method POST removeAlert)
         ]


vinInit :: SnapletInit b Vin
vinInit =
    makeSnaplet "vin" "Some description" Nothing $ do
        as <- liftIO . newMVar $ Alerts M.empty
        addRoutes routes
        return $ Vin as
