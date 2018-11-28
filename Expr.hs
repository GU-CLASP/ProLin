{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyCase #-}
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

data Next v = Here | There {fromThere :: v} deriving (Eq, Functor,Show)
instance Monad Next where
  Here >>= _ = Here
  There x >>= f = f x

type a + b = Either a b

mapLeft :: (a -> c) -> a + b -> c + b
mapLeft f (Left x) = Left (f x)
mapLeft f (Right x) = Right x

pushLeft :: Next (z + w) -> Next z + w
pushLeft = \case
     (Here) -> Left (Here)
     (There (Right x)) -> Right x
     There (Left x) -> Left (There x)

pushRight :: Next (z + w) -> z + Next w
pushRight = \case
     (Here) -> Right(Here)
     (There (Right x)) -> Right (There x)
     There (Left x) -> Left x

class Eq v => Enumerable v where
  splitType :: v -> (forall w. Enumerable w => (v -> Next w) -> (w -> v) -> k) -> k
   -- splitType x $ \f g --- f x = Here ∧ if f x == There y, g y == x
  enumAll :: [v]

instance Enumerable v => Enumerable (Next v) where
  splitType Here k = k @v id There
  splitType (There x) k = splitType x $ \f g ->
    -- HAVE: f x = Here ∧ if f x == There y, g y == x
    -- WANT: f' (There x) = Here ∧ if f' x == There y, g' y == x
    let f' Here = There Here
        f' (There y) = case f y of
          Here -> Here
          There x' -> There (There x')
    in k f' (fmap g)
  enumAll = Here:(There <$> enumAll)

-- >>> type Three = Next (Next (Next (Next Zero)))
-- >>> two :: Three
-- >>> two = There (There Here)
-- >>> splitType two $ \f g -> (g . fromThere . f) (There (There (There Here)))
-- There (There (There Here))

data Zero

instance Enumerable Zero where
  enumAll = []
  splitType = \case

deriving instance (Eq Zero)

instance Show Zero where
  show _ = "<<PANIC:Zero>>"

instance Applicative Next where
  pure = There
  Here <*> _ = Here
  _ <*> Here = Here
  There f <*> There a = There (f a)

data Mult = One | Zero deriving (Eq,Show)

data Exp v where
  Pi :: (String,Mult) -> (Exp v) -> (Exp (Next v)) -> Exp v
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
            f' (Here) = V (Here)
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
                  Here -> pure (Here)
                  There x -> fmap There (f x)


freeVars :: Exp v -> [v]
freeVars = toList

occursIn :: Eq v => v -> Exp v -> Bool
occursIn v e = getAny (foldMap (Any . (== v)) e )

