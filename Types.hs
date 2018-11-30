{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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

module Types where


data Next v = Here | There {fromThere :: v} deriving (Eq, Functor,Show)
instance Monad Next where
  Here >>= _ = Here
  There x >>= f = f x

data Vector v a where
  VNil :: Vector Zero a
  (:*) :: a -> Vector v a -> Vector (Next v) a
  (:+:) :: a -> Vector v a -> Vector w a -> Vector (v + w) a

type a + b = Either a b

(⊗) :: (t1 -> a) -> (t2 -> b) -> (t1, t2) -> (a, b)
(f ⊗ g) (a,b) = (f a, g b)

(⊕) :: (t1 -> a) -> (t2 -> b) -> Either t1 t2 -> Either a b
(f ⊕ _) (Left x) = Left (f x)
(_ ⊕ g) (Right x) = Right (g x)

mapLeft :: (a -> c) -> a + b -> c + b
mapLeft f = f ⊕ id

mapRight :: (t2 -> b) -> Either a t2 -> Either a b
mapRight f = id ⊕ f

pullLeft :: (Next z + w) -> Next (z + w)
pullLeft (Left Here) = Here
pullLeft (Left (There x)) = There (Left x)
pullLeft (Right x) = There (Right x)

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

instance Enumerable String where
  splitType x k = k (\y -> if x == y then Here else There y) id
  enumAll = []


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

