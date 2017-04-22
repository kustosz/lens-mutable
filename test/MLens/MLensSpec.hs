{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MLens.MLensSpec where

import Control.Lens.MLens
import Control.Lens

import Data.IORef
import Test.Hspec              (Spec, describe, it, specify, shouldBe)
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.QuickCheck         (property, Property, Arbitrary)

prop_msetOnMview :: (Arbitrary a, Eq a) => a -> Property
prop_msetOnMview a = monadicIO $ do
    ref <- run $ newIORef a
    run $ mview refVal ref >>= \a -> mset refVal a ref
    res <- run $ readIORef ref
    assert $ res == a

prop_mviewAfterMset :: (Arbitrary a, Eq a) => a -> a -> Property
prop_mviewAfterMset a b = monadicIO $ do
    ref <- run $ newIORef a
    res <- run $ mset refVal b ref >> mview refVal ref
    assert $ res == b

spec :: Spec
spec = do
    describe "mutable lens laws" $ do
        specify "msetting the value to the value obtained from mview does not change the value" $ property $ prop_msetOnMview    @Int
        specify "mviewing the value after msetting yields the value passed to mset"             $ property $ prop_mviewAfterMset @String
    describe "combination with ordinary lenses" $ do
        it "combines with lenses for pairs" $ do
            ref <- newIORef ("Hello", 42)
            mset (refVal . _1) "World" ref
            res <- readIORef ref
            res `shouldBe` ("World", 42)
        it "combines with traversals" $ do
            ref <- newIORef (3, 4)
            mover (refVal . both) succ ref
            res <- mview refVal ref
            res `shouldBe` (4, 5)
