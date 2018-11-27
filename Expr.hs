{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Expr where

import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Monoid

data Next v = Here String | There v deriving (Eq, Functor,Show)
data Zero

deriving instance (Eq Zero)

instance Show Zero where
  show _ = "<<PANIC:Zero>>"

instance Applicative Next where
  pure = There
  Here x <*> _ = Here x
  _ <*> Here x = Here x
  There f <*> There a = There (f a)


data Exp v where
  Pi :: String -> (Exp v) -> (Exp (Next v)) -> Exp v
  App :: [Exp v]  -> Exp v
  Con :: String -> Exp v
  V :: v -> Exp v
  deriving (Eq, Functor,Show)

instance Applicative Exp where
  (<*>) = ap
  pure = V

instance Monad Exp where
  (>>=) :: forall v w. Exp v -> (v -> Exp w) -> Exp w
  e >>= f = case e of
    (V x) -> f x
    (App args) -> App (map (>>= f) args)
    (Con k) -> Con k
    (Pi s dom bod) -> Pi s (dom >>= f) (bod >>= f')
      where f' :: Next v -> Exp (Next w)
            f' (Here x) = V (Here x)
            f' (There x) = fmap There (f x)

instance Foldable Exp where
  foldMap = foldMapDefault

instance Traversable Exp where
  traverse f = \case
    App xs -> App <$> traverse (traverse f) xs
    Con k -> pure (Con k)
    V x -> V <$> f x
    Pi s dom bod -> Pi s <$> traverse f dom <*> traverse f' bod
     where f' = \case
                  Here x -> pure (Here x)
                  There x -> fmap There (f x)


freeVars :: Exp v -> [v]
freeVars = toList

occursIn :: Eq v => v -> Exp v -> Bool
occursIn v e = getAny (foldMap (Any . (== v)) e )

