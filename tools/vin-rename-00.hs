{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
    ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import Data.String
import Data.Maybe
import qualified Database.Redis as R

renames :: M.Map C8.ByteString C8.ByteString
renames = M.fromList [
    ("vin", "car_vin"),
    ("make", "car_make"),
    ("model", "car_model"),
    ("plateNum", "car_plateNum"),
    ("makeYear", "car_makeYear"),
    ("color", "car_color"),
    ("buyDate", "car_buyDate"),
    ("checkupDate", "car_checkupDate"),
    ("cardNumber", "cardNumber_cardNumber"),
    ("validFrom", "cardNumber_validFrom"),
    ("validUntil", "cardNumber_validUntil"),
    ("validUntilMilage", "cardNumber_validUntilMilage"),
    ("milageTO", "cardNumber_milageTO"),
    ("serviceInterval", "cardNumber_serviceInterval"),
    ("cardOwner", "cardNumber_cardOwner"),
    ("manager", "cardNumber_manager"),
    ("owner_name", "contact_ownerName"),
    ("owner_phone1", "contact_ownerPhone1"),
    ("owner_email", "contact_ownerEmail")]

instance Error R.Reply where
    strMsg  = R.Error . fromString
    noMsg = strMsg noMsg

main :: IO ()
main = do
    conn <- R.connect R.defaultConnectInfo
    R.runRedis conn $ do
        (Right vins) <- R.keys "vin:*"
        let
            cnt = length vins
        forM_ (zip [1..] vins) $ \(i, v) -> runErrorT $ do
            liftIO $ putStrLn $ show i ++ "/" ++ show cnt
            ks <- ErrorT $ liftM (fmap (filter (`M.member` renames))) $ R.hkeys v
            vals <- ErrorT $ R.hmget v ks
            ErrorT $ R.hmset v $ map ((renames M.!) *** fromJust) $ filter (isJust . snd) $ zip ks vals
            -- Redis 2.2 can't delete multiple fields
            mapM_ (\k -> ErrorT (R.hdel v [k])) ks
