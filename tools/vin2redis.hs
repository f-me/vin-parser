-- | Uploads vin data to redis
--
-- Usage:
--
-- >vin2redis <program> <file>
--
module Main (
    main
    ) where

import Control.Monad
import qualified Control.Exception as E
import Control.Concurrent
import qualified Data.ByteString.Char8 as C8
import System.Environment
import System.FilePath

import qualified Vin.Import as Vin
import qualified Vin.Models as Vin
import qualified Vin.Model as Vin

main :: IO ()
main = do
    args@(~([p, f])) <- getArgs

    let
        invalids = "errors.csv"
        errlog = "errors.log"

    if length args /= 2
        then putStrLn "Usage: vin2redis <program> <file>"
        else do
            mvar <- newMVar (0, 0)
            _ <- forkIO $ forever $ do
                threadDelay 5000000
                (u, t) <- readMVar mvar
                putStrLn $ "Processed " ++ show t ++ ", uploaded " ++ show u

            Vin.loadFile f invalids errlog (C8.pack p) (Vin.extension $ takeExtension f) (stats mvar)
                `E.catch` onError

            (u, t) <- takeMVar mvar
            mapM_ putStrLn [
                "Processed: " ++ show t,
                "Uploaded: " ++ show u,
                "",
                "Log: " ++ errlog,
                "Invalid data: " ++ invalids]

    where
        stats v u t = void $ swapMVar v (u, t)
        onError :: E.SomeException -> IO ()
        onError = print
