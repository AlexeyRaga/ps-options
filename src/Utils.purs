module Utils
( stringToNumber
, stringToNumberOrZero
, nextExpiry
) where

import Data.Maybe (Maybe(..), maybe)
import Data.Bounded (bottom)
import Data.Enum (fromEnum, succ)
import Data.Date (Date, Month(..), Weekday(..), year, month, weekday, canonicalDate)
import Data.DateTime (DateTime(..), date, adjust)
import Data.Time.Duration (class Duration, Days(..))
import Data.Int (toNumber)
import Global (readFloat, isNaN)
import Prelude (id, (-), (>), (+), ($), (==))

stringToNumber :: String -> Maybe Number
stringToNumber s = if isNaN n then Nothing else Just n where n = readFloat s

stringToNumberOrZero :: String -> Number
stringToNumberOrZero s = maybe 0.0 id (stringToNumber s)

nextExpiry :: Date -> Date
nextExpiry d =
  let sat = expireSaturday d
  in if sat > d then d else expireSaturday (nxtMonth d)

fstInMonth :: Date -> Date
fstInMonth d = canonicalDate (year d) (month d) bottom

nxtMonth :: Date -> Date
nxtMonth d =
  let thisYear  = year d
      thisMonth = month d
   in if thisMonth == December
     then canonicalDate (maybe thisYear id (succ thisYear)) January bottom
     else canonicalDate thisYear (maybe thisMonth id (succ thisMonth)) bottom

expireSaturday :: Date -> Date
expireSaturday d =
  let fstDay = fstInMonth d
      delta = (fromEnum Friday - fromEnum (weekday fstDay))
      fstSat = if delta > 0 then delta else 7 + delta
      dur = Days $ toNumber (fstSat + 14)
  in adjustD dur fstDay

adjustD :: forall d. Duration d => d -> Date -> Date
adjustD d a = maybe a date (adjust d (DateTime a bottom))
