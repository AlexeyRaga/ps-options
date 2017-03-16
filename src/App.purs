module App where

import Data.Maybe
import StockList as StockList
import Stocks as Stocks
import Control.Alt (alt)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import Data.Date (Date)
import Network.HTTP.Affjax (AJAX)
import Prelude (($), (#), show, map, (<>))
import Pux (EffModel, mapState, mapEffects)
import Pux.Html (Html, div, h1, h3, span, p, text, img)
import Pux.Html.Attributes (id_, className, src)

data Action
  = Init
  | StockListAction (StockList.Action)

type State =
  { listState :: StockList.State
  , selectedStock :: Maybe Stocks.Stock
  }

init :: Date -> State
init d =
  { listState: StockList.init
  , selectedStock: Nothing
  }


stockHeader :: Stocks.Stock -> Html Action
stockHeader (Stocks.Stock stock) =
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

stockInfo :: Stocks.Stock -> Html Action
stockInfo (Stocks.Stock stock) =
  div [ className "stock-body" ]
      [ img [ src ("https://chart.finance.yahoo.com/z?&t=6m&q=l&l=on&z=l&a=v&p=m50,m200&s=" <> stock.symbol) ] []
      ]

grid :: forall a. Array (Html a) -> Html a
grid = div [ className "container-fluid" ]

row :: forall a. Array (Html a) -> Html a
row = div [ className "row" ]

view :: State -> Html Action
view state =
  div [ id_ "layout" ]
      [ div [ id_ "stock-list"] [ map StockListAction $ StockList.view state.listState ]
      , div [ id_ "main" ]
            [ maybe (div [] []) stockHeader state.selectedStock
            , maybe (div [] []) stockInfo state.selectedStock
            ]
      ]

update :: Action -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM, now :: NOW)
update (StockListAction (StockList.StockSelected stock)) state =
  StockList.update (StockList.StockSelected stock) state.listState
    # mapState (state { listState = _, selectedStock = Just stock })
    # mapEffects StockListAction

update (StockListAction action) state =
  StockList.update action state.listState
    # mapState (state { listState = _ })
    # mapEffects StockListAction

update Init state = --routeEffects route (state { route = route })
  StockList.update StockList.Init state.listState
    # mapState (state { listState = _ })
    # mapEffects StockListAction
