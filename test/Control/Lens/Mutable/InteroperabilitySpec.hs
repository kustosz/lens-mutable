module Control.Lens.Mutable.InteroperabilitySpec where

import Control.Lens.Mutable
import Control.Lens

import Data.IORef
import Test.Hspec (Spec, describe, it, specify, shouldBe, shouldReturn)

spec :: Spec
spec = do
    describe "left composition with standard lenses" $ do
        describe "getters" $ do
            it "works with lenses" $ do
                ref <- newIORef ("Hello", 42)
                ref ^! referenced . _2 `shouldReturn` 42
            it "works with traversals" $ do
                ref <- newIORef (0, 1)
                ref ^!! referenced . both `shouldReturn` [0, 1]
        describe "setters" $ do
            it "works with lenses" $ do
                ref <- newIORef ("Hello", 42)
                ref & referenced . _1 !~ "World"
                readIORef ref `shouldReturn` ("World", 42)
            it "works with traversals" $ do
                ref <- newIORef (3, 4)
                ref & referenced . both %!~ succ
                readIORef ref `shouldReturn` (4, 5)

    describe "right composition with standard lenses" $ do
        describe "getters" $ do
            it "works with lenses" $ do
                v <- (,) <$> newIORef "Hello" <*> newIORef 42
                v ^! _1 . referenced `shouldReturn` "Hello"
            it "works with traversals" $ do
                v <- (,) <$> newIORef "Hello" <*> newIORef "World"
                v ^!! both . referenced `shouldReturn` ["Hello", "World"]
        describe "setters" $ do
            it "works with lenses" $ do
                v@(ref1, _) <- (,) <$> newIORef "Hello" <*> newIORef 42
                v & _1 . referenced !~ "World"
                readIORef ref1 `shouldReturn` "World"
            it "works with traversals" $ do
                v@(ref1, ref2) <- (,) <$> newIORef 0 <*> newIORef 1
                v & both . referenced %!~ succ
                readIORef ref1 `shouldReturn` 1
                readIORef ref2 `shouldReturn` 2
