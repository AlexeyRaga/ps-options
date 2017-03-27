module App where

import Data.Maybe
import OptionsTable as Options
import StockList as StockList
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import Data.Date (Date)
import Data.Either (Either(..))
import Network.HTTP.Affjax (AJAX)
import Prelude ((#), map, show, ($), (<$>), (<>))
import Pux (Update, mapEffects, mapState, noEffects)
import Pux.Html (Html, div, h1, img, p, span, text, (!))
import Pux.Html.Attributes (id_, className, src)
import Stocks (Options, Stock(..), isSameSymbol, loadOptions, loadStocks, setOptions)

data Action
  = Init
  | StocksLoaded (Either String (Array Stock))
  | StockSelected Stock (Either String Options)
  | StockListAction StockList.Action

type State =
  { date          :: Date
  , stocks        :: Array Stock
  , selectedStock :: Maybe Stock
  , status        :: Maybe String
  , filter        :: String
  }

init :: Date -> State
init d =
  { date: d
  , stocks: []
  , selectedStock: Nothing
  , status: Nothing
  , filter: ""
  }

view :: State -> Html Action
view s =
  div [ id_ "layout" ]
      [ div [ id_ "left-panel" ] [ StockListAction <$> StockList.view s.stocks s.filter ]
      , div [ id_ "main-panel" ]
            [ maybe (span [] []) warning s.status
            , maybe (div [] [])  stockHeader s.selectedStock
            , maybe (div [] []) (stockInfo s.date) s.selectedStock
            ]
      ]

warning :: String -> Html Action
warning msg =
  div [ className "alert alert-warning" ] [ text msg ]

stockHeader :: Stock -> Html Action
stockHeader (Stock stock) =
  div [ className "stock-header"]
      [ h1 [ className "stock-title" ]
           [ text stock.name
           , span [ className "stock-price"] [ text ("$" <> show stock.price) ]
           ]
      , p  [ className "stock-subtitle"]
           [ text (stock.symbol <>" | " <> stock.sector) ]
      ]

stockInfo :: Date -> Stock -> Html Action
stockInfo date s@(Stock stock) =
  div [ className "stock-body" ]
      [ img [ className "stock-chart"
            , src ("https://chart.finance.yahoo.com/z?&t=6m&q=l&l=on&z=l&a=v&p=m50,m200&s=" <> stock.symbol)
            ] []
      , div [ id_ "options-panel" ] [ Options.view date s ]
      ]

update :: Update State Action (ajax :: AJAX, dom :: DOM, now :: NOW)
update Init state =
  { state: state, effects: [ StocksLoaded <$> loadStocks ] }

update (StocksLoaded stocks) state =
  noEffects $ case stocks of
    Right value -> state { stocks = value }
    Left err -> state {
      status = Just $ """Known issue. data.okfn.org is known to be unstable.
                         Please try again in a couple of hours. """ <> err }

update (StockListAction (StockList.StockSelected stock@(Stock s))) state =
  { state: state { selectedStock = Just stock }
  , effects: [ StockSelected stock <$> loadOptions state.date stock ]
  }

update (StockSelected stock opts) state =
  noEffects $ case opts of
    Left err -> state { status = Just err }
    Right opts' ->
      let stock'  = setOptions stock (Just opts')
       in (updateStock state stock') { selectedStock = Just stock', status = Nothing }

update (StockListAction (StockList.SortBy value)) state =
  StockList.update (StockList.SortBy value) state.stocks
  # mapState (state { stocks = _ })
  # mapEffects StockListAction

update (StockListAction (StockList.Filter value)) state =
  StockList.update (StockList.Filter value) state.stocks
  # mapState (\x -> state { filter = value })
  # mapEffects StockListAction

updateStock :: State -> Stock -> State
updateStock state stock = state { stocks = stocks }
  where
    choose new old = if isSameSymbol new old then new else old
    stocks = choose stock <$> state.stocks
