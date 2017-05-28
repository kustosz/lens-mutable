{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Lens.Mutable.Referenced where

import Control.Lens.Mutable.Class
import Data.IORef       (IORef, writeIORef, readIORef)
import Data.STRef       (STRef, writeSTRef, readSTRef)
import Control.Monad.ST (ST)

class Referenced m r where
    referenced :: forall a. MLens' m (r a) a

instance Referenced IO IORef where
    referenced = mlens readIORef $ \a r -> writeIORef r a >> return r

instance Referenced (ST s) (STRef s) where
    referenced = mlens readSTRef $ \a r -> writeSTRef r a >> return r
