{-# LANGUAGE RankNTypes #-}

module Control.Lens.MLens where

import Data.Traversable
import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Const
import Data.IORef
import Control.Lens

type MLens    m s t a b = forall f. Traversable f => (a -> Compose m f b) -> s -> Compose m f t
type AMSetter m s t a b = (a -> Compose m Identity b) -> s -> Compose m Identity t

refVal :: MLens IO (IORef a) (IORef a) a a
refVal mod ref = Compose $ do
    val    <- readIORef ref
    resInF <- getCompose $ mod val
    mapM_ (writeIORef ref) resInF
    return $ ref <$ resInF

mview :: Applicative m => MLens m s t a b -> s -> m a
mview l s = fmap getConst $ getCompose $ l (Compose . pure . Const) s

mset :: Applicative m => AMSetter m s t a b -> b -> s -> m t
mset l a = mover l $ const a

mover :: Applicative m => AMSetter m s t a b -> (a -> b) -> s -> m t
mover l f r = fmap runIdentity $ getCompose $ l (pure . f) r
