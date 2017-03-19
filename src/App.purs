module App where

import Data.Array as A
import Data.Maybe
import OptionsTable as O
import StockList as StockList
import Control.Bind ((=<<))
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import DOM.HTML.HTMLSelectElement (selectedOptions)
import Data.Date (Date)
import Data.Either (Either, either)
import Network.HTTP.Affjax (AJAX)
import Prelude (($), (#), (<$>), (==), const, bind, pure, show, map, id, (<>))
import Pux (EffModel, mapEffects, mapState, noEffects)
import Pux.Html (Html, div, h1, span, p, text, img)
import Pux.Html.Attributes (id_, className, src)
import Stocks (Options, Stock(..), isSameSymbol, loadOptions, loadStocks, setOptions)

data Action
  = Init
  | StocksLoaded (Either String (Array Stock))
  | StockSelected Stock (Either String Options)
  | StockListAction StockList.Action

type State =
  { date :: Date
  , stocks :: Array Stock
  , selectedStock :: Maybe Stock
  }

updateStock :: State -> Stock -> State
updateStock state stock = state { stocks = stocks }
  where
    choose new old = if isSameSymbol new old then new else old
    stocks = choose stock <$> state.stocks

init :: Date -> State
init d =
  { date: d
  , stocks: []
  , selectedStock: Nothing
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
      [ img [ className "stock-chart"
            , src ("https://chart.finance.yahoo.com/z?&t=6m&q=l&l=on&z=l&a=v&p=m50,m200&s=" <> stock.symbol)
            ] []
      ]

view :: State -> Html Action
view state =
  div [ id_ "layout" ]
      [ div [ id_ "stock-list"] [ map StockListAction $ StockList.view state.stocks ]
      , div [ id_ "main" ]
            [ maybe (div [] []) stockHeader state.selectedStock
            , maybe (div [] []) stockInfo state.selectedStock
            , maybe (div [] []) (O.view state.date) state.selectedStock
            ]
      ]

update :: Action -> State -> EffModel State Action (console :: CONSOLE, ajax :: AJAX, dom :: DOM, now :: NOW)
update (StocksLoaded stocks) state =
  noEffects $ state { stocks = either (const []) id stocks }

update (StockSelected stock opts) state =
  let options = either (const Nothing) Just opts
      stock'  = setOptions stock options
   in noEffects $ (updateStock state stock') { selectedStock = Just stock' }

update (StockListAction (StockList.StockSelected stock)) state =
  { state: state { selectedStock = Just stock }
  , effects: [ do
      StockSelected stock <$> loadOptions state.date stock
    ]
  }

update Init state =
  { state: state
  , effects: [ StocksLoaded <$> loadStocks ]
  }
