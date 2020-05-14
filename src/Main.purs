module Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar as Evar
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Widgets.Header (header, footer)
import Routing (match)
import Routing.Hash (getHash, matches)
import Types (MyApp, User)
import Routes (Routes(..), routes, pageForRoute)


routingWidget :: forall a r. MyApp { user :: Ref (Maybe User) |r } a
routingWidget = do
  routeRef <- liftEffect $ do
    var <- Evar.empty
    void $ matches routes \_ route -> void $ Evar.tryPut route var
    pure var
  let awaitRoute = liftAff $ Avar.take routeRef

  liftAff (delay (Milliseconds 0.0))

  route <- liftEffect getHash
  let route' = either (const HomePage) identity (match routes route)

  go awaitRoute route'
  where
  go awaitRoute route = do
    route' <- awaitRoute <|> header route <|> pageForRoute route <|> footer
    go awaitRoute route'


main :: Effect Unit
main = do
  user <- Ref.new Nothing
  runWidgetInDom "root" (runReaderT routingWidget {user})
