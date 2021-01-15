{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericLens where

import GHC.Generics

import Control.Monad.Identity
import Data.Functor.Const

newtype Cost = Cost Double deriving (Show)

data Item = Item { name :: String, cost :: Cost } deriving (Show)

type Lens s t a b = forall f. (Functor f) => (a -> f b) -> (s -> f t)
type Traversal s t a b = forall f. (Applicative f) => (a -> f b) -> (s -> f t)

--type Setter s t a b = (a -> Identity b) -> s -> Identity t

data List a = Empty | Cons a (List a) deriving (Show, Generic)

type M = D1
-- type M = C1
-- type M = S1

type K = Rec0

--data Rep a = Rep a

--class Generic a where
--  type family Rep a :: *
--  from :: a -> Rep a
--  to :: Rep a -> a

class HasTypes_ s t a b where
  types_ :: Traversal s t a b

class GHasTypes s t a b where
  gtypes :: Traversal s t a b

instance (GHasTypes (Rep s s) (Rep t t) a b, Generic s, Generic t) => HasTypes_ s t a b where
  types_ = isoRep . gtypes

--instance GHasTypes s t a b => GHasTypes (D1 m s) (D1 m t) a b where
--  gtypes = isoM . gtypes

instance HasTypes_ Char Char a b where
  types_ _ = pure

instance HasTypes_ Int Int a b where
  types_ _ = pure

isoRep :: (Generic s, Generic t) => Lens s t (Rep s s) (Rep t t)
isoRep f = fmap to . f . from

isoK :: Lens (K1 x a y) (K1 x b y) a b
isoK f s = K1 <$> f (unK1 s)

isoM :: Lens (M1 i c f s) (M1 i c f t) s t
isoM f s = M1 <$> f (unM1 s)

naiveName :: Lens Item Item String String
naiveName a_to_fb (item@Item { name = a }) = (\b -> item { name = b }) <$> a_to_fb a

over :: Traversal s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

update :: Lens s t a b -> b -> s -> t
update l b = runIdentity . l (const (Identity b))

view :: Lens s t a b -> s -> a
view l = getConst . l Const

toListOf :: Traversal s t a b -> s -> [a]
toListOf t = getConst . t (Const . singleton)

singleton :: a -> [a]
singleton x = [x]

item = Item { name = "apple", cost = Cost 1.0 }

main = putStrLn . show $ toListOf naiveName . over naiveName (const "orange") $ item
