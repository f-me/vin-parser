{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Module for field processing
-- Field is Monad, MonadPlus and MonadError,
-- so you can perform any sequenced failable actions
-- @
-- example = do
--     s <- field -- get field value
--     x <- f s -- perform some actions
--     when (p x) $
--         throwError (strMsg "Error")
--     return x
-- @
module Vin.Field (
    Field(..),
    field, verify, alt
    ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader

newtype Field e s a = Field {
    runField :: ErrorT e (Reader s) a }

-- | Get field value
field :: Error e => Field e s s
field = Field $ lift ask

instance Functor (Field e s) where
    fmap f = Field . fmap f . runField

instance Error e => Monad (Field e s) where
    return = Field . return
    x >>= f = Field $ runField x >>= runField . f

instance Error e => MonadError e (Field e s) where
    throwError = Field . throwError
    catchError f h = Field ((runField f) `catchError` (runField . h))

instance Error e => MonadPlus (Field e s) where
    mzero = throwError noMsg
    l `mplus` r = l `catchError` (const r)

-- | Verify field value
verify :: Error e => (a -> Bool) -> (s -> e) -> Field e s a -> Field e s a
verify p msg f = do
    s <- field
    x <- f
    if p x
        then return x
        else throwError (msg s)

-- | Try alternatives
-- Returns result of first succeeded alternative, or fails
-- with message specified
alt :: Error e => e -> [Field e a v] -> Field e a v
alt msg lst = foldr mplus mzero lst `catchError` (const $ throwError msg)
