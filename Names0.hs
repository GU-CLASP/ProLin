{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
module Names0 where

-- import Data.Kind (Constraint)

----------------------------------------------
-- Parametric names

data v :< a = Old a | New v deriving (Functor,Foldable,Traversable)
type a :> v = v :< a


mapNew :: (v -> w) -> (v :< a) -> (w :< a)
mapNew f (New v) = New (f v)
mapNew _ (Old x) = Old x

type Succ a = () :< a



------------------------------------
-- Neutral Scope

newtype Scope tm a = Scope (tm (Succ a))


----------------------------
-- Positive Scope

data EScope' tm a where EScope' ::  v -> tm (v :< a) -> EScope' tm a

toEScope :: tm (Succ a) -> EScope' tm a
toEScope t = EScope' () t

type Term tm = Monad tm

fromEScope :: Term tm => v -> tm (a :> v) -> Scope tm a
fromEScope _ = Scope . fmap (mapNew (const ()))
pattern EScope :: forall tm a. Term tm => forall v. v -> tm (a :> v) -> Scope tm a
pattern EScope x t <- Scope (toEScope -> EScope' x t) where
  EScope x t = fromEScope x t

--------------------------
-- Negative Scope

{-
type UScope tm a = (forall v. v -> tm (a :> v))

fromUscope :: Functor tm => UScope tm a -> Scope tm a
fromUscope f = Scope (f ())

toUscope :: Functor tm => Scope tm a -> (forall v. v -> tm (a :> v))
toUscope (Scope t) = \x -> mapNew (const x) <$> t

pattern UScope :: forall (tm :: * -> *) a. Functor tm => UScope tm a -> Scope tm a
pattern UScope f <- (toUscope -> f) where
  UScope = fromUscope

-----------------------------
-- Transformation functions

lft :: Applicative tm => v -> (a -> tm b) -> (a :> v) -> tm (b :> v)
lft _ f (Old a) = wk (f a)
lft _ _ (New v) = pure (inj v)

substituteOut :: Monad tm => v -> tm a -> tm (a :> v) -> tm a
substituteOut _ t u = u>>= \y -> case y of
  New _ -> t
  Old z -> return z
-}
--------------------------------------
-- Injection class

class v :∈ a where inj :: v -> a
instance v :∈ (a :> v) where inj = New
instance v :∈ a => v :∈ (a :> w) where inj = Old . inj

--------------------------
-- Weakening class

class a :⊆ b where
  injMany :: a -> b
instance a :⊆ b => (a :> v) :⊆ (b :> v) where
  injMany (New v) = New v
  injMany (Old a) = Old (injMany a)
instance a :⊆ b => a :⊆ (b :> v) where
  injMany a = Old (injMany a)
instance a :⊆ a where
  injMany a = a

wk :: (Term f, a :⊆ b) => f a -> f b
wk = fmap injMany



--------------------------------


-- newtype Scope'1 tm a = NEXT'1 (tm (Succ) a)
-- pattern Scope'1 :: forall (tm :: * -> * -> *) a. (Bifunctor tm)
--                => forall tv. tv -> tm (ta :> tv) a -> Scope'1 tm a
-- pattern Scope'1 x t <- NEXT'1 (toEScope1 -> EScope'1 x t) where
--   Scope'1 x t = pack'1 x t
-- data EScope'1 tm a where
--   EScope'1 ::  v -> tm (ta :> v) a -> EScope'1 tm a
-- toEScope1 :: tm (Succ) a -> EScope'1 tm a
-- toEScope1 t = EScope'1 () t
-- pack'1 :: Bifunctor tm => v -> tm (ta :> v) a -> Scope'1 tm a
-- pack'1 _ = NEXT'1 . bimap (mapNew (const ())) id
-- instance Bifunctor p => Bifunctor (Scope'1 p) where
--   bimap f g (NEXT'1 t) = NEXT'1 (bimap (fmap f) g t)
-- instance Bitraversable p => Bifoldable (Scope'1 p) where
--   bifoldMap = bifoldMapDefault
-- instance Bitraversable p => Bitraversable (Scope'1 p) where
--   bitraverse f g (NEXT'1 t) = NEXT'1 <$> (bitraverse (traverse f) g t)

-- newtype Scope'0 tm a = NEXT'0 (tm (Succ a)) deriving (Functor,Foldable,Traversable)
-- pattern Scope'0 :: forall (tm :: * -> * -> *) a. (Bifunctor tm)
--                => forall v. v -> tm (a :> v) -> Scope'0 tm a
-- pattern Scope'0 x t <- NEXT'0 (toEScope0 -> EScope'0 x t) where
--   Scope'0 x t = pack'0 x t
-- data EScope'0 tm a where
--   EScope'0 ::  v -> tm (a :> v) -> EScope'0 tm a
-- toEScope0 :: tm (Succ a) -> EScope'0 tm a
-- toEScope0 t = EScope'0 () t
-- pack'0 :: Bifunctor tm => v -> tm (a :> v) -> Scope'0 tm a
-- pack'0 _ = NEXT'0 . bimap  id (mapNew (const ()))
-- instance Bifunctor p => Bifunctor (Scope'0 p) where
--   bimap f g (NEXT'0 t) = NEXT'0 (bimap f (fmap g) t)
-- instance Bitraversable p => Bifoldable (Scope'0 p) where
--   bifoldMap = bifoldMapDefault
-- instance Bitraversable p => Bitraversable (Scope'0 p) where
--   bitraverse f g (NEXT'0 t) = NEXT'0 <$> (bitraverse (f) (traverse g) t)





