{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
    ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.IO.Class
import Data.List
import Data.String
import qualified Data.ByteString.Char8 as C8
import qualified Carma.ModelTables as MT
import qualified Database.Redis as R

instance Error R.Reply where
    strMsg = R.Error . fromString
    noMsg = strMsg noMsg

main :: IO ()
main = do
    descs <- MT.loadTables
        "resources/site-config/models"
        "resources/site-config/field-groups.json"
    vinFields <- maybe
        (error "Can't find vin model")
        (return . map (fromString . MT.columnName) . MT.tableFields)
        (find ((== "vin") . MT.tableModel) descs)
    conn <- R.connect R.defaultConnectInfo
    R.runRedis conn $ do
        (Right vins) <- R.keys "vin:*"
        let
            cnt = length vins
        forM_ (zip [1..] vins) $ \(i, v) -> runErrorT $ do
            liftIO $ putStrLn $ show i ++ "/" ++ show cnt
            ks <- ErrorT $ liftM (fmap (\\ vinFields)) $ R.hkeys v
            -- Redis 2.2 can't delete multiple fields
            mapM_ (\k -> ErrorT (R.hdel v [k])) ks
