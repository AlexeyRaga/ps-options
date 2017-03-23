module StockList where

import Stocks
import Data.Array (filter, sortBy)
import Data.Ord (comparing)
import Data.String (Pattern(..), contains, toUpper)
import Prelude (const, map, show, ($), (<<<), (<>))
import Pux (EffModel, noEffects)
import Pux.Html (Html, a, div, input, li, p, span, text, ul)
import Pux.Html.Attributes (id_, className, href)
import Pux.Html.Events (onChange, onClick)

type Filter = String
data SortBy = SortBySymbol | SortByPrice

data Action
  = StockSelected Stock
  | SortBy SortBy
  | Filter Filter

view :: Stocks -> Filter -> Html Action
view state txt =
  let stocks' = filter (contains (Pattern txt) <<< stockSymbol) state
   in div [ className "stock-list" ]
      [ cmdPanel state
      , ul [id_ "list", className "stock-list-items nav nav-sidebar"] $ map listItem stocks'
      ]

listItem :: Stock -> Html Action
listItem (Stock state) =
  li  [ className "stock-item", onClick (const $ StockSelected (Stock state)) ]
      [ span [ className "stock-item-symbol"] [text state.symbol ]
      , div  []
             [ span [ className "stock-item-name" ] [ text state.name ]
             , p [ className "stock-item-sector" ]
                 [ text state.sector
                 , span [ className "stock-item-price"] [ text ("$" <> show state.price) ]
                 ]
             ]
  ]

cmdPanel :: Stocks -> Html Action
cmdPanel state =
  div [ className "stock-list-commands" ]
      [ input [ onChange (\evt -> Filter (toUpper evt.target.value) )] []
      , span []
             [ a [ className "btn glyphicon glyphicon-usd"
                 , href "#", onClick (const $ SortBy SortByPrice)
                 ] []
             , a [ className "btn glyphicon glyphicon-sort-by-alphabet"
                 , href "#", onClick (const $ SortBy SortBySymbol)
                 ] []
             ]
      ]

update :: forall eff. Action -> Stocks -> EffModel Stocks Action eff
update (SortBy SortByPrice)  = noEffects <<< sortBy (comparing stockPrice)
update (SortBy SortBySymbol) = noEffects <<< sortBy (comparing stockSymbol)
update _ = noEffects
