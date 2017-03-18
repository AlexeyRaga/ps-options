module Options
-- ( nextExpiry
-- , loadOptions
-- , fakeLoadOptions
-- , Options(..), Option(..)
-- )
where

import Stocks
import Control.Monad ((>=>))
import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Parser (jsonParser)
import Data.Bounded (bottom)
import Data.Date (Date, Month(..), Weekday(..), year, month, day, weekday, canonicalDate)
import Data.DateTime (DateTime(..), date, adjust)
import Data.Either (Either(Left, Right), either)
import Data.Enum (fromEnum, succ)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (class Duration, Days(..))
import Network.HTTP.Affjax (AJAX, URL, get)
import Prelude (id, show, (-), (>), (>=), (+), ($), (==), (<>), (<<<), (<$>), bind, pure)
import Utils (stringToNumberOrZero)

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

keyRx :: Regex
keyRx = unsafeRegex "(\\w+):" global

loadOptions :: forall eff. Date -> Stock -> Aff (ajax :: AJAX | eff) (Either String Options)
loadOptions d s = do
  res <- attempt $ get (urlFor d s)
--  let fixed r = replace keyRx "\"$1\":" r.response
  let decode r = decodeJson r.response :: Either String Options
  let stocks = either (Left <<< show) decode res
  pure stocks

urlFor :: Date -> Stock -> URL
urlFor d (Stock s) =
  let exp  = nextExpiry d
      expY = year exp
      expM = month exp
      expD = day exp
  in "https://www.google.com/finance/option_chain?q=" <> s.symbol
     <> "&expd=" <> (show $ fromEnum expD)
     <> "&expm=" <> (show $ fromEnum expM)
     <> "&expy=" <> (show $ fromEnum expY)
     <> "&output=json"

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

adjustD :: forall d. Duration d => d -> Date -> Date
adjustD d a = maybe a date (adjust d (DateTime a bottom))

expireSaturday :: Date -> Date
expireSaturday d =
  let fstDay = fstInMonth d
      delta = (fromEnum Friday - fromEnum (weekday fstDay))
      fstSat = if delta > 0 then delta else 7 + delta
      dur = Days $ toNumber (fstSat + 14)
  in adjustD dur fstDay
