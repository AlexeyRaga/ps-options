module App where

import Data.Maybe
import OptionsTable as O
import StockList as StockList
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import Data.Date (Date)
import Data.Either (Either(..), either)
import Network.HTTP.Affjax (AJAX)
import Prelude (const, id, map, show, ($), (<$>), (<>))
import Pux (EffModel, noEffects)
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
  , status :: String
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
  , status: ""
  }

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

stockInfo :: Stock -> Html Action
stockInfo (Stock stock) =
  div [ className "stock-body" ]
      [ img [ className "stock-chart"
            , src ("https://chart.finance.yahoo.com/z?&t=6m&q=l&l=on&z=l&a=v&p=m50,m200&s=" <> stock.symbol)
            ] []
      ]

stockInfo' :: Date -> Stock -> Html Action
stockInfo' date s@(Stock stock) =
  div [ className "stock-body" ]
      [ img [ className "stock-chart"
            , src ("https://chart.finance.yahoo.com/z?&t=6m&q=l&l=on&z=l&a=v&p=m50,m200&s=" <> stock.symbol)
            ] []
      , div [ id_ "options-panel" ] [ O.view date s ]
      ]

view :: State -> Html Action
view state =
  div [ id_ "layout" ]
      [ div [ id_ "stock-list"] [ map StockListAction $ StockList.view state.stocks ]
      , div [ id_ "main" ]
            [ maybe (div [] []) stockHeader state.selectedStock
            , maybe (div [] []) (stockInfo' state.date) state.selectedStock
            -- , div [id_ "options-panel"]
            --       [ maybe (div [] []) (O.view state.date) state.selectedStock ]
            ]
      , div [ id_ "status" ] [ text $ state.status ]
      ]

update :: Action -> State -> EffModel State Action (console :: CONSOLE, ajax :: AJAX, dom :: DOM, now :: NOW)
update Init state =
  { state: state, effects: [ StocksLoaded <$> loadStocks ] }

update (StocksLoaded stocks) state =
  noEffects $ state { stocks = either (const []) id stocks }

update (StockListAction (StockList.StockSelected stock)) state =
  { state: state { selectedStock = Just stock }
  , effects: [ StockSelected stock <$> loadOptions state.date stock ]
  }

update (StockSelected stock opts) state =
  noEffects $ case opts of
    Left err -> state { status = err }
    Right opts' ->
      let stock'  = setOptions stock (Just opts')
       in (updateStock state stock') { selectedStock = Just stock', status = "" }
  -- let options = either (const Nothing) Just opts
  --     stock'  = setOptions stock options
  --  in noEffects $ (updateStock state stock') { selectedStock = Just stock' }
