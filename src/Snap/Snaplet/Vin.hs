{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Data.Map (Map)
import           Data.Map as M
import           Data.Maybe
import           System.Directory (getTemporaryDirectory)

import           Control.Monad.CatchIO (finally, catch)
import           Control.Monad.State
import           Data.Lens.Template
import           Data.Text (Text)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Util.FileUploads
import           System.Posix.Files (createLink, removeLink)

import           Vin.Import


data Vin = Vin
    { _state :: MVar (Map String Text)
    }

makeLens ''Vin


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

    s <- gets _state

    liftIO $ modifyMVar_ s $ \m ->
        return $ M.insert f "Обработка..." m

    liftIO $ forkIO $ do
        loadFile f' fError program $ partContentType info

        modifyMVar_ s $ \m ->
            return $ M.update (const $ Just "Закончено!") f m

        `E.catches` [ E.Handler (\ (ex :: VinUploadException) ->
                                     modifyMVar_ s $ \m -> do
                                         return $ M.update (const . Just $ vReason ex) f m
                                )
                    , E.Handler (\ (ex :: E.SomeException) ->
                                     modifyMVar_ s $ \m -> do
                                         return $ M.update (const . Just $  T.pack $ show ex) f m
                                )
                    ]

        `finally` removeLink f'

    writeBS "Ok" -- send to client
  where
    fError = "resources/static/error.csv"
    f' = f ++ "-link"


getState :: Handler b Vin ()
getState = do
    s <- gets _state
    res <- liftIO $ readMVar s
    putResponse $ setContentType "text/html; charset=utf-8" $ emptyResponse

    mapM_ (\(_,msg) -> writeText $ T.concat [ "<div class='alert alert-info'>\
                                              \<a class='close' data-dismiss='alert'>x</a>"
                                            , msg
                                            , "</div>"
                                            ]
          ) $ M.toList res


routes :: [(ByteString, Handler b Vin ())]
routes = [ ("/upload", method POST upload)
         , ("/state", method POST getState)
         ]


vinInit :: SnapletInit b Vin
vinInit =
    makeSnaplet "vin" "Some description" Nothing $ do
        s <- liftIO $ newMVar M.empty
        addRoutes routes
        return $ Vin s
