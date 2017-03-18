module Stocks where

import Utils
import Control.Monad.Aff (Aff, attempt)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Either (Either(Left), either)
import Network.HTTP.Affjax (AJAX, get)
import Prelude (($), (<<<), show, bind, pure)

type Stocks = Array Stock

newtype Stock = Stock
  { symbol :: String
  , name   :: String
  , sector :: String
  , price  :: Number
  , low52  :: Number
  , high52 :: Number
  }

instance decodeJsonStock :: DecodeJson Stock where
  decodeJson json = do
    obj    <- decodeJson json
    symbol <- obj .? "Symbol"
    name   <- obj .? "Name"
    sector <- obj .? "Sector"
    price  <- obj .? "Price"
    low52  <- obj .? "52 week low"
    high52 <- obj .? "52 week high"
    pure $ Stock { symbol : symbol
                 , name   : name
                 , sector : sector
                 , price  : stringToNumberOrZero price
                 , low52  : stringToNumberOrZero low52
                 , high52 : stringToNumberOrZero high52
                 }

loadStocks :: forall eff. Aff (ajax :: AJAX | eff) (Either String Stocks)
loadStocks = do
  res <- attempt $ get "http://data.okfn.org/data/core/s-and-p-500-companies/r/constituents-financials.json"
  let decode r = decodeJson r.response :: Either String Stocks
  pure $ either (Left <<< show) decode res
