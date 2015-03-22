{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Monad.Open.Class
( MonadOpen(..)
, Op(..)
) where

import Control.Applicative
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
