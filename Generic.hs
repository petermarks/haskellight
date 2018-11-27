{-# language DataKinds, DeriveFunctor, GADTs, KindSignatures, MultiParamTypeClasses, TypeOperators #-}

module Generic where

import Prelude hiding (take, drop)
import Control.Applicative

------------------------------------------------------------------------
-- HVect

data HVect :: [*] -> * where
  HNil :: HVect '[]
  (:*:) :: t -> HVect ts -> HVect (t ': ts)

infixr 5 :*:



------------------------------------------------------------------------
-- IVect

data IVect :: (* -> *) -> [*] -> * where
  INil :: IVect f '[]
  (:&:) :: f t -> IVect f ts -> IVect f (t ': ts)

infixr 5 :&:



------------------------------------------------------------------------
-- Stream

data Stream a = a :$: Stream a
  deriving (Functor)

infixr 5 :$:

instance Applicative Stream where
  pure a = let as = a :$: as in as
  (f :$: fs) <*> (v :$: vs) = f v :$: (fs <*> vs)

instance (Semigroup c) => Semigroup (Stream c) where
  (<>) = liftA2 (<>)

instance (Monoid c) => Monoid (Stream c) where
  mempty = pure mempty

cycleList :: [a] -> Stream a
cycleList as = let as' = foldr (:$:) as' as in as'

drop :: Int -> Stream a -> Stream a
drop n (_ :$: as) | n > 0 = drop (n - 1) as
drop _ as         = as

take :: Int -> Stream a -> [a]
take n (a :$: as) | n > 0 = a : take (n - 1) as
take _ _          = []

unfold :: (a -> (b, a)) -> a -> Stream b
unfold f a = let (b, a') = f a in b :$: unfold f a'
