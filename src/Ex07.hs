module Ex07
  ( getInitialState
  , cartHandler
  , updateCartHandler
  ) where

import qualified Data.Aeson   as Aeson
import           GHC.Generics (Generic)
import qualified Zero.Server  as Server

data CartItem =
  CartItem
    { model    :: String
    , quantity :: Int
    }
  deriving (Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

newtype Cart =
  Cart [CartItem]
  deriving (Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

getInitialState :: Cart
getInitialState = Cart []

cartHandler :: Cart -> Server.Request -> (Cart, Server.Response)
cartHandler cart _ = (cart, Server.jsonResponse items)
  where
    Cart items = cart

updateCartHandler :: Cart -> Server.Request -> (Cart, Server.Response)
updateCartHandler cart req = response
  where
    body = Server.requestBody req
    response =
      case Server.decodeJson body of
        Left err -> (cart, Server.failureResponse err)
        Right newItem ->
          (addItemToCart cart newItem, Server.stringResponse "ok")

addItemToCart :: Cart -> CartItem -> Cart
addItemToCart (Cart items) item = Cart (items ++ [item])
