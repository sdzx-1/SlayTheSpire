{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Game.HList where

import Control.Applicative (Const (Const))
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Constraint, Type)

type I = Identity
type C = Const

data H f (xs :: [Type]) where
  N :: H f '[]
  (:-) :: f t -> H f xs -> H f (t ': xs)

infixr 5 :-

instance FAll Show f xs => Show (H f xs) where
  show N = "[]"
  show (a :- N) = "[" ++ show a ++ "]"
  show (a :- b) = "[" ++ show a ++ "," ++ drop 1 (show b)

-- >>> show (I 1 :- I "nice" :- I True :- N)
-- "[Identity 1,Identity \"nice\",Identity True]"

type family
  FAll
    (c :: Type -> Constraint)
    (f :: Type -> Type)
    (xs :: [Type])
    :: Constraint
  where
  FAll _ _ '[] = ()
  FAll c f (x ': xs) = (c (f x), FAll c f xs)

type family
  HAll
    (c :: Type -> Constraint)
    (xs :: [Type])
    :: Constraint
  where
  HAll _ '[] = ()
  HAll c (x ': xs) = (c x, HAll c xs)

pattern I :: a -> Identity a
pattern I a = Identity a
{-# COMPLETE I #-}

pattern C :: a -> Const a b
pattern C a = Const a
{-# COMPLETE C #-}
