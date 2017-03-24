module StockList where

import Stocks
import Data.Array (filter, sortBy)
import Data.Ord (comparing)
import Data.String (Pattern(..), contains, toUpper)
import Prelude (const, map, show, ($), (<<<), (<>))
import Pux (Update, noEffects)
import Pux.Html (Html, a, div, input, li, p, span, text, ul)
import Pux.Html.Attributes (id_, className, href)
import Pux.Html.Events (onChange, onClick)

type Filter = String
data SortBy = SortBySymbol | SortByPrice

data Action
  = StockSelected Stock
  | SortBy SortBy
  | Filter Filter

view :: Array Stock -> Filter -> Html Action
view stocks txt =
  let stocks' = filter (contains (Pattern txt) <<< stockSymbol) stocks
   in div [ className "stock-list" ]
      [ cmdPanel
      , ul [id_ "list", className "stock-list-items nav nav-sidebar"] $ map listItem stocks'
      ]

listItem :: Stock -> Html Action
listItem (Stock stock) =
  li  [ className "stock-item", onClick (const $ StockSelected (Stock stock)) ]
      [ span [ className "stock-item-symbol"] [text stock.symbol ]
      , div  []
             [ span [ className "stock-item-name" ] [ text stock.name ]
             , p [ className "stock-item-sector" ]
                 [ text stock.sector
                 , span [ className "stock-item-price"] [ text ("$" <> show stock.price) ]
                 ]
             ]
  ]

cmdPanel :: Html Action
cmdPanel =
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

update :: forall eff. Update (Array Stock) Action eff
update (SortBy SortByPrice)  = noEffects <<< sortBy (comparing stockPrice)
update (SortBy SortBySymbol) = noEffects <<< sortBy (comparing stockSymbol)
update _ = noEffects
