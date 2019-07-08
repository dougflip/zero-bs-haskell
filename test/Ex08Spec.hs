module Ex08Spec
  ( spec
  ) where

import qualified Ex08
import           Test.Hspec

spec :: Spec
spec = do
  describe "Ex08" $ do
    it "provides an empty initial state" $ do
      let actual = Ex08.toSortedList Ex08.getInitialState
      actual `shouldBe` []
    it "adds a new item to the cart" $ do
      let newItem = Ex08.CartItem "Laptop" 1
      let actual =
            Ex08.toSortedList $ Ex08.updateCart Ex08.getInitialState newItem
      length actual `shouldBe` 1
