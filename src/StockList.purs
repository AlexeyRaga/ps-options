module StockList where

import Stocks
import Control.Monad.Aff (attempt)
import Data.Argonaut (decodeJson)
import Data.Either (Either(Left, Right), either)
import Network.HTTP.Affjax (AJAX, get)
import Prelude (($), show, const, map, bind, pure, (<<<), (<>))
import Pux (EffModel, noEffects)
import Pux.Html (Html, (!), (#>), a, ul, li, h5, p, div, span, button, text)
import Pux.Html.Attributes (id_, href, className)
import Pux.Html.Events (onClick)
import DOM (DOM)

type Stocks = Array Stock

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

update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (ReceiveStocks (Left err)) state =
  noEffects $ state { status = "Error fetching todos: " <> show err }

update (ReceiveStocks (Right stocks)) state =
  noEffects $ state { stocks = stocks, status = "Ready." }

update (StockSelected stock) state =
  noEffects state

update Init state =
  { state: state { status = "Fetching stocks list..." }
  , effects: [ do
      res <- attempt $ get "http://data.okfn.org/data/core/s-and-p-500-companies/r/constituents-financials.json"
      let decode r = decodeJson r.response :: Either String Stocks
      let todos = either (Left <<< show) decode res
      pure $ ReceiveStocks todos
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
