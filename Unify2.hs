{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Unify2 where

import Expr
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad
type a + b = Either a b

data Constraint v w where
  -- v : meta variables to unify
  -- z : variables that can unify only to themselves
  -- w : variables that we can unify to
  (:=:) :: forall v w z. Eq z => Exp (v+(z+w)) -> Exp (z+w) -> Constraint v w


class Eq v => Enumerable v where
  decide :: v -> (v -> Next w,Next w -> v)

h1 :: Exp (Next (v + (z + w))) -> Exp (v + (Next z + w))
h1 = fmap $ \case
  There (Left x) -> Left x
  There (Right (Right x)) -> Right (Right x)
  There (Right (Left x)) -> Right (Left (There x))
  Here s -> Right (Left (Here s))

h2 :: Exp (Next (z + w)) -> Exp (Next z + w)
h2 = fmap $ \case
     (Here s) -> Left (Here s)
     (There (Right x)) -> Right x
     There (Left x) -> Left (There x)

applyOneSubst :: forall v v' w. Eq v => (v -> Next v') -> Exp w -> Constraint v w ->Constraint v' w
applyOneSubst d t (u :=: v) = (u >>= f) :=: v
  where f :: v + (z + w) -> Exp (v' + (z + w))
        f (Left x) = case d x of
             Here _ -> (Right . Right) <$> t
             There x' -> V (Left x')
        f (Right w) = V (Right w)

unify :: forall v w. Enumerable v => Eq w => [Constraint v w] -> Maybe (Subst v w)
-- Invariant: the substitution is already applied
unify [] = Just []
unify ((App args1 :=: App args2):constraints)
  | length args1 == length args2 = unify (zipWith (:=:) args1 args2++constraints) 
  | otherwise = Nothing
unify ((Pi _ dom bod :=: Pi _ dom' bod'):constraints)
  = unify ((dom:=:dom'):(h1 bod :=:h2 bod'):constraints)
unify ((V (Right x) :=: V x'):constraints)
  | x == x' = unify constraints
  | x /= x' = Nothing
unify ((V (Left x) :=: t):constraints)
  -- | x `occursIn` t = Nothing
  -- | otherwise
  = case sequenceA t of
      Left _ -> Nothing
      Right t' -> ((x,t') :) <$> unify (map (applyOneSubst into t') constraints)
  where (into,outof) = decide x
unify (_:_) = Nothing

-- | Identity (nothing is substituted)
-- idSubst = M.empty


unify2 :: forall v w. Eq w => Enumerable v => Exp (v+w) -> Exp w -> Maybe (Subst v w)
unify2 s t = unify @v @w [(:=:) @v @w @Zero ((Right <$>) <$> s)  (Right <$> t)]

liftW :: Applicative m => (v -> m v) -> Either w v -> m (Either w v)
liftW f (Right v) = Right <$> f v
liftW _ (Left v) = pure (Left v)

type Subst v z = [(v,Exp z)]

data PSubst a w where
  PSubst :: (a -> a' + w) -> Subst a' w -> PSubst a w
