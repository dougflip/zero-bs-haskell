module Ex08
  ( getCartHandler
  , updateCartHandler
  , getInitialState
  , updateCart
  , toSortedList
  , CartItem(CartItem)
  ) where

import qualified Data.Aeson      as Aeson
import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import           GHC.Generics    (Generic)
import qualified Zero.Server     as Server

{-|
  Handler logic
-}
getCartHandler :: Cart -> Server.Request -> (Cart, Server.Response)
getCartHandler cart _ = (cart, Server.jsonResponse $ toSortedList cart)

updateCartHandler :: Cart -> Server.Request -> (Cart, Server.Response)
updateCartHandler cart req = response
  where
    body = Server.requestBody req
    response =
      case Server.decodeJson body of
        Left err -> (cart, Server.failureResponse err)
        Right newItem -> (updateCart cart newItem, Server.stringResponse "ok")

{-|
  Cart API
-}
data CartItem =
  CartItem
    { model    :: String
    , quantity :: Int
    }
  deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

newtype Cart =
  Cart (Map.Map String CartItem)
  deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

getInitialState :: Cart
getInitialState = Cart Map.empty

updateCart :: Cart -> CartItem -> Cart
updateCart (Cart stuff) item =
  Cart $ Map.alter (upsertItem item) (model item) stuff

upsertItem :: CartItem -> Maybe CartItem -> Maybe CartItem
upsertItem item Nothing = Just item
upsertItem newItem (Just existingItem) =
  Just $ existingItem {quantity = (quantity newItem + quantity existingItem)}

toSortedList :: Cart -> [CartItem]
toSortedList (Cart cart) = sortedAscByQuantity
  where
    items = Map.elems cart
    sortedAscByQuantity = reverse $ List.sortOn quantity items
