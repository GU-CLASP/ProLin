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

data Constraint v w where
  -- v : meta variables to unify
  -- z : variables that can unify only to themselves
  -- w : variables that we can unify to
  (:=:) :: forall v w z. Eq z => Exp (v+(z+w)) -> Exp (z+w) -> Constraint v w


h1 :: Exp (Next (v + (z + w))) -> Exp (v + (Next z + w))
h1 = fmap (fmap pushLeft . pushRight)

h2 :: Exp (Next (z + w)) -> Exp (Next z + w)
h2 = fmap pushLeft

applyOneSubst :: forall v v' w. Eq v => (v -> Next v') -> Exp w -> Constraint v w ->Constraint v' w
applyOneSubst d t (u :=: v) = (u >>= f) :=: v
  where f :: v + (z + w) -> Exp (v' + (z + w))
        f (Left x) = case d x of
             Here -> (Right . Right) <$> t
             There x' -> V (Left x')
        f (Right w) = V (Right w)

-- | A partial substitution from v to t. Unsubstituted variables are left in v'
data PSubs v t where
  PSubs :: Enumerable v' => (v' -> v) -> (v -> Exp (v' + t)) -> PSubs v t

applySubst :: PSubs v w -> Exp (v+w) -> (forall v'. Enumerable v' => Exp (v'+w) -> k) -> k
applySubst (PSubs _out subs) e k = k $ e >>= \case
                                        Left x -> subs x
                                        Right x -> V (Right x)

unify :: forall v w. Enumerable v => Eq w => [Constraint v w] -> Maybe (PSubs v w)
-- Invariant: the substitution is already applied
unify [] = Just (PSubs id (V . Left))
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
  = splitType x $ \into outof -> case sequenceA t of
      Left _ -> Nothing
      Right t' -> flip fmap (unify (map (applyOneSubst into t') constraints)) $ \case
        (PSubs outof' f) -> PSubs (outof . outof') $ \x' -> case into x' of
          There y -> f y
          Here -> fmap Right t'
unify (_:_) = Nothing

-- | Identity (nothing is substituted)
-- idSubst = M.empty


unify2 :: forall v w. Eq w => Enumerable v => Exp (v+w) -> Exp w -> Maybe (PSubs v w)
unify2 s t = unify @v @w [(:=:) @v @w @Zero ((Right <$>) <$> s)  (Right <$> t)]

