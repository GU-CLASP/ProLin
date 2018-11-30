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
import Types
import Expr
import Control.Monad
data Constraint v w where
  -- z : variables that we can not unify to
  -- v : meta variables to unify
  -- w : variables that we can unify to
  (:=:) :: forall v w z. Eq z => Exp (z+(v+(w))) -> Exp (z+(v+w)) -> Constraint v w



applyOneSubst :: forall v v' w. Eq v => (v -> Next v') -> Exp (v'+w) -> Constraint v w -> Constraint v' w
applyOneSubst d t (u :=: v) = (u >>= f) :=: (v >>= f)
  where f :: z + (v + w) -> Exp (z + (v' + w))
        f (Right (Left x)) = case d x of
             Here -> Right <$> t -- substitute
             There x' -> V (Right (Left x'))
        f (Left x)= V (Left x)
        f (Right (Right x)) = V (Right (Right x))

-- | A partial substitution from v to t. Unsubstituted variables are left in v'
data PSubs v t where
  PSubs :: Enumerable v' => (v' -> v) ->
                            (v -> Next v') -> -- substitued variables mapped to "Here"
                            (v -> Exp (v' + t)) -> -- apply substitution.
                            PSubs v t

showSubs :: (Show t, Show v) => [v] -> (PSubs v t) -> String
showSubs xs (PSubs o _i s) = unlines [show x ++ " => " ++ show (mapLeft o <$> (s x)) | x <- xs]

applySubst :: PSubs v w -> Exp (v+w) -> (forall v'. Enumerable v' => Exp (v'+w) -> k) -> k
applySubst (PSubs _ _ subs) e k = k $ e >>= \case
                                        Left x -> subs x
                                        Right x -> V (Right x)

unify :: forall v w. Enumerable v => Eq w => [Constraint v w] -> Maybe (PSubs v w)
-- Invariant: the substitution is already applied
unify [] = Just (PSubs id There (V . Left))
unify ((Con x :=: Con x'):constraints)
  | x == x' = unify constraints
  | x /= x' = Nothing
unify ((App args1 :=: App args2):constraints)
  | length args1 == length args2 = unify (zipWith (:=:) args1 args2++constraints) 
  | otherwise = Nothing
unify ((Pi _ dom bod :=: Pi _ dom' bod'):constraints)
  = unify ((dom:=:dom'):(h bod :=:h bod'):constraints)
     where h :: Exp (Next (z + (v + w))) -> Exp (Next z + (v + w))
           h = fmap pushLeft
unify ((V (Left x) :=: V (Left x')):constraints) -- completely fixed vars; must be equal
  | x == x' = unify constraints
  | x /= x' = Nothing
unify ((V (Right (Right x)) :=: V (Right (Right x'))):constraints) -- two fixed vars
  | x == x' = unify constraints
  | x /= x' = Nothing
unify ((V (Right (Left x)) :=: t):constraints) -- metavar
  = splitType x $ \into outof ->  -- find the variable that we want to substitute (Here; iso is given by into;outof)
    case sequenceA t of  -- there cannot be any non-unifyable variables in substituted term.
      Left _ -> Nothing -- (or it would escape)
      Right t' -> case sequenceA (pullLeft . mapLeft into <$> t') of -- occurs check
       Here -> Nothing
       There t'' -> flip fmap (unify (map (applyOneSubst into t'') constraints)) $ \case
        (PSubs outof' into' f) -> -- compose the substitutions
          PSubs (outof . outof') (into' <=< into) $ \x' ->
             (case into x' of
                Here -> t''
                There v' -> V (Left v')
                ) >>= \case
                    Left v' -> f v'
                    Right w -> V (Right w)
unify ((t :=: V (Right (Left x))):constraints) = unify ((V (Right (Left x)) :=: t):constraints)
unify (_:_) = Nothing


unify2 :: forall v w. Eq w => Enumerable v => Exp (v+w) -> Exp (v+w) -> Maybe (PSubs v w)
unify2 s t = unify @v @w [(:=:) @v @w @Zero (Right <$> s)  (Right <$> t)]

Just test0 = unify2 @String @() (V (Left "a")) (V (Left "b"))
Just test1 = unify2 (V (Left "a")) (V (Right "x"))
Just test2 = unify2 (V (Left "a")) (App [V (Left "x"), V (Right "y")])
Just test3 = unify2 (App [(Con "arst"),(V (Left "a"))]) (App [V (Left "b"), V (Right "y")])
Just test4 = unify2 (App [(V (Right "arst")),(V (Left "a")),V (Left "a")]) (App [V (Left "b"), V (Left "c"), V (Left "b")])

-- >>> putStrLn $ showSubs ["a","b","c"] test4
-- "a" => V (Right "arst")
-- "b" => V (Right "arst")
-- "c" => V (Right "arst")
