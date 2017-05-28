{-# LANGUAGE RankNTypes #-}

module Control.Lens.Mutable.Class where

import Data.Traversable
import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Const
import Control.Lens

type MLens     m   s t a b = forall f. Traversable f => (a -> Compose m f b) -> s -> Compose m f t
type MLens'    m   s   a   = MLens m s s a a

type MLensLike m f s t a b = (a -> Compose m f b) -> s -> Compose m f t
type AMSetter  m   s t a b = MLensLike m Identity  s t a b
type MGetting  m r s   a   = MLensLike m (Const r) s s a a
type AMGetter  m   s   a   = MGetting m a s a

mlens :: Monad m => (s -> m a) -> (b -> s -> m t) -> MLens m s t a b
mlens get set f s = Compose $ get s >>= getCompose . f >>= mapM (flip set s)

infixl 8 ^!
mview :: Applicative m => AMGetter m s a -> s -> m a
mview l s = fmap getConst $ getCompose $ l (Compose . pure . Const) s
(^!) :: Applicative m => s -> AMGetter m s a -> m a
(^!) = flip mview

mset, (!~) :: Applicative m => AMSetter m s t a b -> b -> s -> m t
mset l a = mover l $ const a
(!~) = mset

mover, (%!~) :: Applicative m => AMSetter m s t a b -> (a -> b) -> s -> m t
mover l f r = fmap runIdentity $ getCompose $ l (pure . f) r
(%!~) = mover
