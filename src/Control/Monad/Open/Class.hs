{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Monad.Open.Class
( MonadOpen(..)
, Op(..)
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe

import qualified Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy

import Data.Monoid

-- | To know @'MonadOpen' a b m@ is to have a function @a → m b@ which serves
-- as a recursive call to the ambient open operation. The @'MonadOpen' a b m@
-- judgement presupposes @'Monad' m@.
--
class Monad m ⇒ MonadOpen a b m | m → a b where
  -- | A recursive call to the ambient open operation.
  call ∷ a → m b

-- | The type of open operations from @a@ to @b@ in modality @m@; when
-- @'Alternative' m@ is evident, then such operations may be composed
-- horizontally via a 'Monoid' instance.
newtype Op a m b = Op { _op ∷ a → m b }

instance Alternative m ⇒ Monoid (Op a m b) where
  mempty = Op $ \_ → empty
  mappend (Op f) (Op g) = Op $ \x → f x <|> g x

instance MonadOpen a b m ⇒ MonadOpen a b (ReaderT r m) where
  call = lift . call
instance (Monoid w, MonadOpen a b m) ⇒ MonadOpen a b (Strict.WriterT w m) where
  call = lift . call
instance (Monoid w, MonadOpen a b m) ⇒ MonadOpen a b (Lazy.WriterT w m) where
  call = lift . call
instance MonadOpen a b m ⇒ MonadOpen a b (Strict.StateT s m) where
  call = lift . call
instance MonadOpen a b m ⇒ MonadOpen a b (Lazy.StateT s m) where
  call = lift . call
instance (Monoid w, MonadOpen a b m) ⇒ MonadOpen a b (Strict.RWST r w s m) where
  call = lift . call
instance (Monoid w, MonadOpen a b m) ⇒ MonadOpen a b (Lazy.RWST r w s m) where
  call = lift . call
instance MonadOpen a b m ⇒ MonadOpen a b (ExceptT e m) where
  call = lift . call
instance MonadOpen a b m ⇒ MonadOpen a b (IdentityT m) where
  call = lift . call
instance MonadOpen a b m ⇒ MonadOpen a b (ContT r m) where
  call = lift . call
instance MonadOpen a b m ⇒ MonadOpen a b (ListT m) where
  call = lift . call
instance MonadOpen a b m ⇒ MonadOpen a b (MaybeT m) where
  call = lift . call
