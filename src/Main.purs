module Main where

import Prelude

import Auth (setupUserRef)
import Concur.Core (Widget(..))
import Concur.React (HTML)
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar as Evar
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Routes (Routes(..), routes, pageForRoute)
import Routing (match)
import Routing.Hash (getHash, matches)
import Types (MyApp, User)
import Widgets.Header (header, footer)


routingWidget :: forall a . Widget HTML a
routingWidget = do
  routeRef <- liftEffect $ do
    var <- Evar.empty
    void $ matches routes \_ route -> void $ Evar.tryPut route var
    pure var
  let awaitRoute = liftAff $ Avar.take routeRef

  liftAff (delay (Milliseconds 0.0))

  route <- liftEffect getHash
  let route' = either (const HomePage) identity (match routes route)

  user <- setupUserRef
  runReaderT (go awaitRoute route') {user}
  where
  go awaitRoute route = do
    route' <- awaitRoute <|> header route <|> pageForRoute route <|> footer
    go awaitRoute route'


main :: Effect Unit
main = do
  runWidgetInDom "root" routingWidget
