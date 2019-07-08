import qualified Ex08
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests =
  testGroup
    "Ex08"
    [ testCase "it provides an empty initial state" $ do
        let actual = Ex08.toSortedList Ex08.getInitialState
        actual @?= []
    , testCase "it adds a new item to the cart" $ do
        let newItem = Ex08.CartItem "Laptop" 1
        let actual =
              Ex08.toSortedList $ Ex08.updateCart Ex08.getInitialState newItem
        length actual @?= 1
    ]
