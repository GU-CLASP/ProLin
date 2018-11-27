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
type a + b = Either a b

data Constraint v w where
  -- v : meta variables to unify
  -- z : variables that can unify only to themselves
  -- w : variables that we can unify to
  (:=:) :: forall v w z. Eq z => Exp (v+(z+w)) -> Exp (z+w) -> Constraint v w


class Eq v => Enumerable v where
  splitType :: v -> (forall w. Enumerable w => (v -> Next w) -> (Next w -> v) -> k) -> k
  decide :: v -> Zero + v

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

data MayS v t where
  NothingS :: MayS v t
  JustS :: (v -> Exp (v' + t)) -> MayS v t


unify :: forall v w. Enumerable v => Eq w => [Constraint v w] -> MayS v w
-- Invariant: the substitution is already applied
unify [] = JustS (V . Left)
unify ((App args1 :=: App args2):constraints)
  | length args1 == length args2 = unify (zipWith (:=:) args1 args2++constraints) 
  | otherwise = NothingS
unify ((Pi _ dom bod :=: Pi _ dom' bod'):constraints)
  = unify ((dom:=:dom'):(h1 bod :=:h2 bod'):constraints)
unify ((V (Right x) :=: V x'):constraints)
  | x == x' = unify constraints
  | x /= x' = NothingS
unify ((V (Left x) :=: t):constraints) 
  -- | x `occursIn` t = Nothing
  -- | otherwise
  = splitType x $ \into outof -> case sequenceA t of
      Left _ -> NothingS
      Right t' -> case unify (map (applyOneSubst into t') constraints) of
        NothingS -> NothingS
        JustS f -> JustS $ \x' -> case into x' of
          There y -> f y
          Here _ -> fmap Right t'
unify (_:_) = NothingS

-- | Identity (nothing is substituted)
-- idSubst = M.empty


unify2 :: forall v w. Eq w => Enumerable v => Exp (v+w) -> Exp w -> MayS v w
unify2 s t = unify @v @w [(:=:) @v @w @Zero ((Right <$>) <$> s)  (Right <$> t)]

