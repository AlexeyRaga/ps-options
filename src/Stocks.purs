module Stocks where

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Prelude (($), bind, pure, id)
import Data.Maybe (Maybe(..), maybe)
import Global (readFloat, isNaN)

newtype Stock = Stock
  { symbol :: String
  , name   :: String
  , sector :: String
  , price  :: Number
  , low52  :: Number
  , high52 :: Number }

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

stringToNumber :: String -> Maybe Number
stringToNumber s = if isNaN n then Nothing else Just n where n = readFloat s
{-# INLINE stringToNumber #-}

stringToNumberOrZero :: String -> Number
stringToNumberOrZero s = maybe 0.0 id (stringToNumber s)
