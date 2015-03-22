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
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.RWS.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid

-- | A concrete structure implementing the 'MonadOpen' signature.
--
newtype OpenT a b m b'
  = OpenT
  { _openT ∷ ReaderT (a → m b) m b'
  } deriving (Applicative, Functor, Monad, Alternative, MonadPlus, MonadIO, MonadCont, MonadFix)

instance MonadTrans (OpenT a b) where
  lift = OpenT . lift

instance MonadWriter w m ⇒ MonadWriter w (OpenT a b m) where
  tell = OpenT . tell
  listen = OpenT . listen . _openT
  pass = OpenT . pass. _openT

instance MonadReader r m ⇒ MonadReader r (OpenT a b m) where
  ask = OpenT $ lift ask
  local f (OpenT x) = OpenT . ReaderT $ local f . (runReaderT x)

instance MonadState s m ⇒ MonadState s (OpenT a b m) where
  get = OpenT $ get
  put = OpenT . put

instance MonadError e m ⇒ MonadError e (OpenT a b m) where
  throwError = OpenT . throwError
  catchError (OpenT m) = OpenT . catchError m . (_openT .)

instance MonadRWS r w s m ⇒ MonadRWS r w s (OpenT a b m)

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
