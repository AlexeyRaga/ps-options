module App where

import Data.Maybe
import OptionsTable as O
import StockList as StockList
import Control.Bind ((=<<))
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import DOM.HTML.HTMLSelectElement (selectedOptions)
import Data.Date (Date)
import Data.Either (either)
import Network.HTTP.Affjax (AJAX)
import Options (Option(..), Options(..), loadOptions)
import Prelude (($), (#), const, bind, pure, show, map, id, (<>))
import Pux (EffModel, mapEffects, mapState, noEffects)
import Pux.Html (Html, div, h1, span, p, text, img)
import Pux.Html.Attributes (id_, className, src)
import Stocks (Stock(..), Stocks, loadStocks)

newtype StockInfo = StockInfo
  { stock :: Stock
  , calls :: Array Option
  , puts  :: Array Option
  }
  
data Action
  = Init
  | StockSelected Stock (Maybe Options)
  | StockListAction StockList.Action

type State =
  { date :: Date
  , listState :: StockList.State
  , selectedStock :: Maybe Stock
  , selectedOptions :: Maybe Options
  }

init :: Date -> State
init d =
  { date: d
  , listState: StockList.init
  , selectedStock: Nothing
  , selectedOptions: Nothing
  }


stockHeader :: Stock -> Html Action
stockHeader (Stock stock) =
  div [ className "stock-header"]
      [ div []
            [ h1 [ className "stock-title" ]
                 [ text stock.name
                 , span [ className "stock-price"] [ text ("$" <> show stock.price) ]
                 ]
            , p  [ className "stock-subtitle"]
                 [ text (stock.symbol <>" | " <> stock.sector) ]
            ]
      ]

stockInfo :: Stock -> Html Action
stockInfo (Stock stock) =
  div [ className "stock-body" ]
      [ img [ src ("https://chart.finance.yahoo.com/z?&t=6m&q=l&l=on&z=l&a=v&p=m50,m200&s=" <> stock.symbol) ] []
      ]

view :: State -> Html Action
view state =
  div [ id_ "layout" ]
      [ div [ id_ "stock-list"] [ map StockListAction $ StockList.view state.listState ]
      , div [ id_ "main" ]
            [ maybe (div [] []) stockHeader state.selectedStock
            , maybe (div [] []) stockInfo state.selectedStock
            , maybe (div [] []) (O.view state.date) state.selectedOptions
            ]
      ]

update :: Action -> State -> EffModel State Action (console :: CONSOLE, ajax :: AJAX, dom :: DOM, now :: NOW)
update (StockSelected stock opts) state =
  noEffects (state { selectedStock = Just stock, selectedOptions = opts })
  -- StockList.update (StockList.StockSelected stock) state.listState
  --   # mapState (state { listState = _, selectedStock = Just stock })
  --   # mapEffects StockListAction

update (StockListAction (StockList.StockSelected stock)) state =
  { state: state { selectedStock = Just stock }
  , effects: [ do
      options <- loadOptions state.date stock
      pure $ StockSelected stock (either (const Nothing) Just options)
    ]
  }

update (StockListAction action) state =
  StockList.update action state.listState
    # mapState (state { listState = _ })
    # mapEffects StockListAction

update Init state = --routeEffects route (state { route = route })
  StockList.update StockList.Init state.listState
    # mapState (state { listState = _ })
    # mapEffects StockListAction
