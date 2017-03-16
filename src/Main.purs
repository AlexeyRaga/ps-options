module Main where

import App as App
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Data.DateTime (date)
import Data.DateTime.Instant (toDateTime)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit, bind, pure, (<<<), (<$>))
import Pux (start, renderToDOM, Config, CoreEffects)
import Signal ((~>))
import Pux.Router (sampleUrl)

type AppEffects = (dom :: DOM, ajax :: AJAX, now :: NOW)

-- | App configuration
config :: forall eff. App.State -> Eff (dom :: DOM | eff) (Config App.State App.Action AppEffects)
config state = do
  urlSignal <- sampleUrl

  let routeSignal = urlSignal ~> \r -> App.Init

  pure
    { initialState: state
    , update: App.update
    , view: App.view
    , inputs: [routeSignal] }

main :: Eff (CoreEffects AppEffects) Unit
main = do
  date <- (date <<< toDateTime) <$> now
  app <- start =<< config (App.init date)

  renderToDOM "#app" app.html
