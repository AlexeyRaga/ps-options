module OptionsTable
where

import Data.Bounded (bottom)
import Data.Date (Date, Month(..), canonicalDate)
import Data.DateTime
import Data.Enum (fromEnum)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Time.Duration (Days(..))
import Prelude (Unit, show, (-), (>), (+), (<$>), ($))
import Pux.Html (Html, table, tr, td, div, h1, span, p, text, img)
import Pux.Html.Attributes (id_, className, src)
import Stocks (Option(..), Options(..), Stock(..))

view :: forall a. Date -> Stock -> Html a
view d (Stock s) =
  div [ className "options-panel" ]
      [ div [ className "options-header" ]
            [ text (show d)
            , maybe (div [][]) (\(Options os) -> optionsTable os.calls) s.options
            ]
      ]

optionsTable :: forall a. Array Option -> Html a
optionsTable os =
  table [] (optionRow <$> os)

optionRow :: forall a. Option -> Html a
optionRow (Option o) =
  tr []
     [ td [] [ text $ show o.price ]
     ]
