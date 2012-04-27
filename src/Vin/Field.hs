{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Module for field processing
module Vin.Field (
    Field(..),
    field, verify
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
    l `mplus` r = l `catchError` (\e -> r)

-- | Verify field value
verify :: Error e => (a -> Bool) -> (s -> e) -> Field e s a -> Field e s a
verify p msg f = do
    s <- field
    x <- f
    if p x
        then return x
        else throwError (msg s)
