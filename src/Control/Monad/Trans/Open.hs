{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Monad.Trans.Open
( OpenT
, OpenT'
  -- ** Operations
, close
) where

import Control.Monad.Open.Class
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Monoid
import Data.Function

-- | A concrete structure implementing the 'MonadOpen' signature.
--
newtype OpenT a b m b'
  = OpenT
  { _openT ∷ ReaderT (a → m b) m b'
  } deriving (Applicative, Functor, Monad, Alternative, MonadIO)

instance MonadTrans (OpenT a b) where
  lift x = OpenT $ lift x

instance MonadWriter w m ⇒ MonadWriter w (OpenT a b m) where
  tell x = lift $ tell x
  listen (OpenT x) = OpenT $ listen x
  pass (OpenT x) = OpenT $ pass x

-- | A simplified version of the 'OpenT' type which fixes the output parameter.
type OpenT' a m b = OpenT a b m b

instance Monad m ⇒ MonadOpen a b (OpenT a b m) where
  call x = do
    rec ← OpenT ask
    OpenT . lift $ rec x

-- | An open operation may be closed.
--
close
  ∷ Monad m
  ⇒ Op a (OpenT a b m) b
  → a
  → m b
close o@(Op f) x =
  runReaderT (_openT $ f x) $ close o
