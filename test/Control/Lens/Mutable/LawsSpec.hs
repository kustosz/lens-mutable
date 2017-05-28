{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Lens.Mutable.LawsSpec where

import Control.Lens.Mutable
import Control.Lens

import Data.IORef
import Test.Hspec              (Spec, describe, specify)
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.QuickCheck         (property, Property, Arbitrary)

prop_msetOnMview :: (Arbitrary a, Eq a) => a -> Property
prop_msetOnMview a = monadicIO $ do
    ref <- run $ newIORef a
    run $ ref ^! referenced >>= \v -> ref & referenced !~ v
    res <- run $ readIORef ref
    assert $ res == a

prop_mviewAfterMset :: (Arbitrary a, Eq a) => a -> a -> Property
prop_mviewAfterMset a b = monadicIO $ do
    ref <- run $ newIORef a
    res <- run $ mset referenced b ref >> mview referenced ref
    assert $ res == b

prop_doubleMset :: (Arbitrary a, Eq a) => a -> a -> a -> Property
prop_doubleMset init a b = monadicIO $ do
    ref <- run $ newIORef init
    run $ mset referenced a ref >> mset referenced b ref
    res <- run $ readIORef ref
    assert $ res == b

spec :: Spec
spec = do
    describe "mutable lens laws" $ do
        specify "msetting the value based on mview doesn't change it" $ property $ prop_msetOnMview    @Int
        specify "mviewing the value after msetting gives it back"     $ property $ prop_mviewAfterMset @Int
        specify "when msetting the value twice, the second wins"      $ property $ prop_doubleMset     @Int
