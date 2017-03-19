module Stocks where

import Utils
import Control.Monad.Aff (Aff, attempt)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Date (Date, year, month, day)
import Data.Either (Either(Left), either)
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Network.HTTP.Affjax (AJAX, URL, get)
import Prelude (($), (<<<), (==), (<>), show, bind, pure)

type Stocks = Array Stock

newtype Option = Option
  { price  :: Number
  , bid    :: Number
  , ask    :: Number
  , strike :: Number
  }
newtype Options = Options
  { puts  :: Array Option
  , calls :: Array Option
  }

newtype Stock = Stock
  { symbol  :: String
  , name    :: String
  , sector  :: String
  , price   :: Number
  , options :: Maybe Options
  }

stockOptions :: Stock -> Maybe Options
stockOptions (Stock s) = s.options

setOptions :: Stock -> Maybe Options -> Stock
setOptions (Stock stock) o = Stock $ stock { options = o }

isSameSymbol :: Stock -> Stock -> Boolean
isSameSymbol (Stock a) (Stock b) = a.symbol == b.symbol

instance decodeJsonStock :: DecodeJson Stock where
  decodeJson json = do
    obj    <- decodeJson json
    symbol <- obj .? "Symbol"
    name   <- obj .? "Name"
    sector <- obj .? "Sector"
    price  <- obj .? "Price"
    pure $ Stock { symbol  : symbol
                 , name    : name
                 , sector  : sector
                 , price   : stringToNumberOrZero price
                 , options : Nothing
                 }

instance decodeJsonOption :: DecodeJson Option where
  decodeJson json = do
    obj    <- decodeJson json
    price  <- obj .? "p"
    bid    <- obj .? "b"
    ask    <- obj .? "a"
    strike <- obj .? "s"
    pure $ Option { price  : stringToNumberOrZero price
                  , bid    : stringToNumberOrZero bid
                  , ask    : stringToNumberOrZero ask
                  , strike : stringToNumberOrZero strike
                  }

instance decodeJsonOptions :: DecodeJson Options where
  decodeJson json = do
    obj   <- decodeJson json
    puts  <- obj .? "puts"
    calls <- obj .? "calls"
    pure $ Options { puts: puts, calls: calls }

loadStocks :: forall eff. Aff (ajax :: AJAX | eff) (Either String Stocks)
loadStocks = do
  res <- attempt $ get "http://data.okfn.org/data/core/s-and-p-500-companies/r/constituents-financials.json"
  let decode r = decodeJson r.response :: Either String Stocks
  pure $ either (Left <<< show) decode res

loadOptions :: forall eff. Date -> Stock -> Aff (ajax :: AJAX | eff) (Either String Options)
loadOptions d s = do
  res <- attempt $ get (optionsUrl d s)
--  let fixed r = replace keyRx "\"$1\":" r.response
  let decode r = decodeJson r.response :: Either String Options
  let stocks = either (Left <<< show) decode res
  pure stocks

optionsUrl :: Date -> Stock -> URL
optionsUrl d (Stock s) =
  let expY = year d
      expM = month d
      expD = day d
  in "https://www.google.com/finance/option_chain?q=" <> s.symbol
     <> "&expd=" <> (show $ fromEnum expD)
     <> "&expm=" <> (show $ fromEnum expM)
     <> "&expy=" <> (show $ fromEnum expY)
     <> "&output=json"

fixupKeys :: String -> String
fixupKeys = replace (unsafeRegex "(\\w+):" global) "\"$1\":"
