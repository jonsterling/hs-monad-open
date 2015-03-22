{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | This is a demonstration of using open recursion to implement the
-- judgements of a small theory modularly.
module Control.Monad.Trans.Open.Example
( -- * Syntax
  Ty(..)
, Tm(..)
  -- * Judgement Forms
, J(..)
  -- * Theories
, Theory
, unitThy
, prodThy
, combinedThy
  -- * Result
, judge
  -- ** Injecting Effects
, traceThy
, Pack(..)
, tracedJudge
) where

import Control.Applicative
import Control.Monad.Open.Class
import Control.Monad.Trans.Open
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Monoid

-- | The syntax of terms in our theory.
data Ty
  = Unit
  | Prod Ty Ty
  deriving (Eq, Show)

-- | The syntax of terms in our theory.
--
data Tm
  = Ax
  | Pair Tm Tm
  deriving (Eq, Show)

-- | Next, the forms of judgements are inductively defined. We index the
-- 'J' type by its synthesis.
--
--   ['DisplayTy'] To know @'DisplayTy' α@ is to know the textual notation for the type @α@.
--   ['DisplayTm'] To know @'DisplayTm' m@ is to know the textual notation for the term @m@.
--   ['Check'] To know @'Check' α m@ is to know that @m@ is a canonical verification of @α@.
--
data J a where
  DisplayTy ∷ Ty → J String
  DisplayTm ∷ Tm → J String
  Check ∷ Ty → Tm → J Bool

deriving instance Show (J a)
-- | A @'Theory' j@ is an open, partial implementation of the judgements
-- defined by the judgement signature @j@. Theories may be composed, since
-- @'Monoid' ('Theory' j)@ holds.
--
type Theory j
  = ( Monad m
    , Alternative m
    , MonadOpen (j a) a m
    )
  ⇒ Op (j a) m a

-- | A 'Theory' implementing the judgements as they pertain to the 'Unit' type former.
--
unitThy ∷ Theory J
unitThy = Op $ \case
  DisplayTy Unit → return "unit"
  DisplayTm Ax → return "ax"
  Check Unit m → return $ m == Ax
  _ → empty

-- | A 'Theory' implementing the judgments as they pertain to the 'Prod' type former.
--
prodThy ∷ Theory J
prodThy = Op $ \case
  DisplayTy (Prod a b) → do
    x ← call $ DisplayTy a
    y ← call $ DisplayTy b
    return $ "(" ++ x ++ " * " ++ y ++ ")"
  DisplayTm (Pair m n) → do
    x ← call $ DisplayTm m
    y ← call $ DisplayTm n
    return $ "<" ++ x ++ ", " ++ y ++ ">"
  Check (Prod a b) mn →
    case mn of
      Pair m n → (&&) <$> call (Check a m) <*> call (Check b n)
      _ → return False
  _ → empty

-- | The horizontal composition of the two above theories.
--
-- @
-- 'combinedThy' = 'unitThy' '<>' 'prodThy'
-- @
--
combinedThy ∷ Theory J
combinedThy = unitThy <> prodThy

-- | Judgements may be tested through the result of closing the theory.
--
-- @
-- 'judge' = 'close' 'combinedThy'
-- @
--
-- >>> judge $ DisplayTy $ Prod Unit (Prod Unit Unit)
-- "(unit * (unit * unit))"
--
-- >>> judge $ DisplayTm $ Pair Ax (Pair Ax Ax)
-- "<ax, <ax, ax>>"
--
-- >>> judge $ Check (Prod Unit Unit) Ax
-- False
--
-- >>> judge $ Check (Prod Unit (Prod Unit Unit)) (Pair Ax (Pair Ax Ax))
-- True
--
judge = close combinedThy

data Pack φ = forall a. Pack (φ a)

instance Show (Pack J) where
  show (Pack j) = show j

-- | We can inject a log of all assertions into the theory!
--
traceThy
  ∷ ( Monad m
    , Alternative m
    , MonadOpen (j a) a m
    , MonadWriter [Pack J] m
    )
  ⇒ Op (J a) m a
traceThy = Op $ \j → do
  tell $ [Pack j]
  empty

tracedJudge ∷ J b → (Maybe b, [Pack J])
tracedJudge j = runIdentity . runWriterT . runMaybeT $ (close $ traceThy <> combinedThy) j
