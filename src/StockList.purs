module StockList where

import Stocks
import Options
import Control.Monad.Aff.Console (CONSOLE)
import Data.Either (Either(Left, Right))
import Network.HTTP.Affjax (AJAX)
import Prelude (($), show, const, map, bind, pure, (<>))
import Pux (EffModel, noEffects)
import Pux.Html (Html, ul, li, p, div, span, text)
import Pux.Html.Attributes (id_, className)
import Pux.Html.Events (onClick)

data Action
  = Init
  | ReceiveStocks (Either String Stocks)
  | StockSelected Stock

type State =
  { stocks :: Stocks
  , status :: String
  }

init :: State
init = { stocks: [], status: "Waiting for data to be loaded" }

update :: forall eff. Action -> State -> EffModel State Action (console :: CONSOLE, ajax :: AJAX | eff)
update (ReceiveStocks (Left err)) state =
  noEffects $ state { status = "Error fetching stocks: " <> show err }

update (ReceiveStocks (Right stocks)) state =
  noEffects $ state { stocks = stocks, status = "Ready." }

update (StockSelected stock) state =
  noEffects state

update Init state =
  { state: state { status = "Fetching stocks list..." }
  , effects: [ do
      stocks <- loadStocks
      pure $ ReceiveStocks stocks
    ]
  }

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

view :: State -> Html Action
view state =
  ul [id_ "list", className "nav nav-sidebar"] $ map listItem state.stocks
