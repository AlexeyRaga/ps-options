module OptionsTable
where

import Data.DateTime
import Stocks
import Options
import Data.Bounded (bottom)
import Data.Date (Date, Month(..), canonicalDate)
import Data.Enum (fromEnum)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Time.Duration (Days(..))
import Prelude (Unit, show, (-), (>), (+), ($))
import Pux.Html (Html, div, h1, span, p, text, img)
import Pux.Html.Attributes (id_, className, src)

view :: forall a. Date -> Options -> Html a
view d o =
  div [ className "options-panel" ]
      [ div [ className "options-header" ]
            [ text (show d)]

      ]
