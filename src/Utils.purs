module Utils
( stringToNumber
, stringToNumberOrZero
) where

import Data.Maybe (Maybe(..), maybe)
import Global (readFloat, isNaN)
import Prelude (id)

stringToNumber :: String -> Maybe Number
stringToNumber s = if isNaN n then Nothing else Just n where n = readFloat s

stringToNumberOrZero :: String -> Number
stringToNumberOrZero s = maybe 0.0 id (stringToNumber s)
