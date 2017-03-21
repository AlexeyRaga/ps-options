module OptionsTable
where

import Data.DateTime
import Data.Map (lookup)
import Data.Maybe (Maybe(..), maybe)
import Prelude (show, ($), (<$>), (>=))
import Pux.Html (Html, col, colgroup, div, table, tbody, td, text, th, thead, tr, (!), (#), (##))
import Pux.Html.Attributes (className, span_)
import Stocks (Option(Option), Options(Options), Stock(Stock), Strike)

newtype TableRow = TableRow
  { call   :: Maybe Option
  , strike :: Strike
  , put    :: Maybe Option
  }

view :: forall a. Date -> Stock -> Html a
view d stock@(Stock s) =
  div [ className "options-panel" ]
      [ div [ className "options-header" ]
            [ text (show d)
            , maybe (div ## []) (optionsTable stock) s.options
            ]
      ]

optionsTable :: forall a. Stock -> Options -> Html a
optionsTable s os =
  table []
    [ colgroup []
        [ col [ span_ "3", className "options-columns-calls" ] []
        , col [ className "options-columns-strike" ] []
        , col [ span_ "3", className "options-columns-puts" ] []
        ]
    , thead [] ((\x -> th # text x) <$> ["Price", "Bid", "Ask", "Strike", "Price", "Bid", "Ask"])
    , tbody [] (optionRow s <$> toRows os)
    ]

optionRow :: forall a. Stock -> TableRow -> Html a
optionRow (Stock s) (TableRow r) =
  tr [ className cls ]
     [ td ! className "option-cell-call" # value _.price r.call
     , td ! className "option-cell-call" # value _.bid   r.call
     , td ! className "option-cell-call" # value _.ask   r.call
     , td # text $ show r.strike
     , td ! className "option-cell-put" # value _.price r.put
     , td ! className "option-cell-put" # value _.bid   r.put
     , td ! className "option-cell-put" # value _.ask   r.put
     ]
  where
    cls = if s.price >= r.strike then "option-strike-low" else "option-strike-high"
    value f o = text $ case o of
      Nothing -> ""
      Just (Option o') -> show $ f o'

toRows :: Options -> Array TableRow
toRows (Options o) = toRow <$> o.strikes
  where
    toRow s = TableRow { call: lookup s o.calls, strike: s, put: lookup s o.puts }
