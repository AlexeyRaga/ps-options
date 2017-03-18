module Main where

import App as App
import Control.Bind ((=<<))
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW, now)
import DOM (DOM)
import Data.DateTime (date)
import Data.DateTime.Instant (toDateTime)
import Network.HTTP.Affjax (AJAX)
import Options (fakeLoadOptions)
import Prelude (Unit, bind, pure, show, (<$>), (<<<), ($))
import Pux (start, renderToDOM, Config, CoreEffects)
import Pux.Router (sampleUrl)
import Signal ((~>))

type AppEffects = (console :: CONSOLE, dom :: DOM, ajax :: AJAX, now :: NOW)

-- | App configuration
config :: forall eff. App.State -> Eff (console :: CONSOLE, dom :: DOM | eff) (Config App.State App.Action AppEffects)
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
