module StockList where

import Stocks
import Control.Monad.Aff.Console (CONSOLE)
import Network.HTTP.Affjax (AJAX)
import Prelude (($), show, const, map, (<>))
import Pux (EffModel, noEffects)
import Pux.Html (Html, ul, li, p, div, span, text)
import Pux.Html.Attributes (id_, className)
import Pux.Html.Events (onClick)

data Action = StockSelected Stock

update :: forall eff. Action -> Stocks -> EffModel Stocks Action (console :: CONSOLE, ajax :: AJAX | eff)

update (StockSelected stock) state = noEffects state

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

view :: Stocks -> Html Action
view state =
  ul [id_ "list", className "nav nav-sidebar"] $ map listItem state
