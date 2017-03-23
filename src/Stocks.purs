module Stocks
( Stock(..), Stocks, Strike
, Option(..), Options(..)
, loadStocks
, loadOptions
, isSameSymbol
, setOptions
, stockPrice, stockSymbol
) where

import Utils
import Control.Monad.Aff (Aff, attempt)
import Data.Argonaut (class DecodeJson, decodeJson, jsonParser, (.?))
import Data.Array (head)
import Data.Date (Date)
import Data.Either (Either(..), either)
import Data.Int (fromNumber)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, URL, get)
import Prelude (bind, pure, show, ($), (<$>), (<<<), (<>), (==), (>>=))

type Stocks = Array Stock
type Strike = Number

newtype Option = Option
  { price  :: Number
  , bid    :: Number
  , ask    :: Number
  , strike :: Strike
  }
newtype Options = Options
  { puts    :: Map Strike Option
  , calls   :: Map Strike Option
  , strikes :: Array Strike
  }

newtype Stock = Stock
  { symbol  :: String
  , name    :: String
  , sector  :: String
  , price   :: Number
  , options :: Maybe Options
  }

stockPrice :: Stock -> Number
stockPrice (Stock s) = s.price

stockSymbol :: Stock -> String
stockSymbol (Stock s) = s.symbol

optStrike :: Option -> Strike
optStrike (Option o) = o.strike

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
    price  <- obj .? "lastPrice"
    bid    <- obj .? "bid"
    ask    <- obj .? "ask"
    strike <- obj .? "strike"
    pure $ Option { price  : price
                  , bid    : bid
                  , ask    : ask
                  , strike : strike
                  }

instance decodeJsonOptions :: DecodeJson Options where
  decodeJson json = do
    obj     <- decodeJson json
    chain   <- obj    .? "optionChain"
    res     <- (chain .? "result")  >>= eitherHead
    strikes <- res    .? "strikes"
    opts    <- (res   .? "options") >>= eitherHead
    puts    <- opts   .? "puts"
    calls   <- opts   .? "calls"
    pure $ Options { puts: toMap puts, calls: toMap calls, strikes: strikes }
    where
      toMap :: Array Option -> Map Strike Option
      toMap opts = fromFoldable $ (\o -> Tuple (optStrike o) o) <$> opts


eitherHead :: forall a. Array a -> Either String a
eitherHead a = maybe (Left "Noooo") Right (head a)

loadStocks :: forall eff. Aff (ajax :: AJAX | eff) (Either String Stocks)
loadStocks = do
  res <- attempt $ get "http://data.okfn.org/data/core/s-and-p-500-companies/r/constituents-financials.json"
  let decode r = decodeJson r.response :: Either String Stocks
  pure $ either (Left <<< show) decode res

loadOptions :: forall eff. Date -> Stock -> Aff (ajax :: AJAX | eff) (Either String Options)
loadOptions d s = do
  res <- attempt $ get (optionsUrl d s)
  let fixed = mapEither show (\r -> fixupKeys r.response) res
  let stocks = fixed >>= jsonParser >>= decodeJson
  pure stocks

optionsUrl :: Date -> Stock -> URL
optionsUrl d (Stock s) =
    "https://query2.finance.yahoo.com/v7/finance/options/" <> s.symbol
     <> "?date=" <> show (numSecs $ toSeconds d)
  where
    numSecs (Seconds ts) = fromMaybe 0 (fromNumber ts)

fixupKeys :: String -> String
fixupKeys = replace (unsafeRegex "(\\w+):" global) "\"$1\":"
