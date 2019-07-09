import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Ex08
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden          (findByExtension, goldenVsString)
import           Test.Tasty.HUnit           (testCase, (@?=))

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

addLaptop :: Ex08.Cart -> Ex08.Cart
addLaptop = Ex08.updateCart $ Ex08.CartItem "Laptop" 1

addPhone :: Ex08.Cart -> Ex08.Cart
addPhone = Ex08.updateCart $ Ex08.CartItem "Phone" 1

cartAsJson :: [Ex08.CartItem] -> IO LBS.ByteString
cartAsJson = pure . Aeson.encode

unitTests =
  testGroup
    "Ex08"
    [ testCase "it provides an empty initial state" $ do
        let actual = Ex08.toSortedList Ex08.getInitialState
        actual @?= []
    , goldenVsString
        "it adds one item to a cart"
        "./test/golden-files/adds-item-to-cart.json" $ do
        let cart = addLaptop Ex08.getInitialState
        cartAsJson $ Ex08.toSortedList cart
    , goldenVsString
        "it increments the quantity of an existing item"
        "./test/golden-files/increment-item.json" $ do
        let cart = addLaptop $ addLaptop Ex08.getInitialState
        cartAsJson $ Ex08.toSortedList cart
    , goldenVsString
        "it adds 2 different items"
        "./test/golden-files/add-two-items.json" $ do
        let cart = addPhone $ addLaptop Ex08.getInitialState
        cartAsJson $ Ex08.toSortedList cart
    ]
