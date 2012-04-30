{-# LANGUAGE OverloadedStrings #-}
-- | Module for row processing.
-- The main goal is that processing of every field may fail,
-- but processing of row is Applicative (and Alternative),
-- so you can't sequence actions on row.
-- Combined row action can collect all errors instead of failing
-- on first invalid field.
-- 
-- For example, this code:
-- > (,) <$> column "key" string <*> column "value" int
-- will try to convert `value' field even if first field fail
-- But, of course, final result will be Nothing
-- 
-- To process list of fields you can use sequenceA
-- > sequenceA [column "key" string, column "value" int, ...]
module Vin.Row (
    RowError(..),
    Row,
    row,
    column
    ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.Map as M
import Data.List (nub)

import Vin.Field

data RowError k e = NoColumn k | FieldError k e
    deriving (Eq, Ord, Read, Show)

newtype Row k e a v = Row {
    runRow :: ReaderT (M.Map k a) (Writer [RowError k e]) (Maybe v) }

-- | Execute row
row :: (Eq k, Eq e) => M.Map k a -> Row k e a v -> Either [RowError k e] v
row m r = case runWriter (runReaderT (runRow r) m) of
    (Just x, _) -> Right x
    (Nothing, es) -> Left $ nub es

instance Functor (Row k e a) where
    fmap f = Row . fmap (fmap f) . runRow

instance Applicative (Row k e a) where
    pure = Row . pure . pure
    f <*> x = Row $ do
        f' <- runRow f
        x' <- runRow x
        return (f' <*> x')

instance Alternative (Row k e a) where
    empty = Row $ return Nothing
    l <|> r = Row $ do
        l' <- runRow l
        r' <- runRow r
        return (l' <|> r')        

-- | Process field at column
-- Collect errors from fields processing
column :: Ord k => k -> Field e a v -> Row k e a v
column key f = Row $ do
    m <- ask
    case M.lookup key m of
        Nothing -> do
            lift $ tell [NoColumn key]
            return Nothing
        Just x -> case runReader (runErrorT (runField f)) x of
            Left msg -> do
                lift $ tell [FieldError key msg]
                return Nothing
            Right val -> return (Just val)
