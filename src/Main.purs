module Main where

import Prelude

import Auth (setupUserRef)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (either)
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Data.Variant as V
import Effect (Effect)
import Effect.AVar as Evar
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Page.Article (articlePage)
import Page.Home (homePage)
import Page.LogIn (logIn)
import Page.Profile (profilePage)
import Routes (Routes(..), routes, toPath)
import Routing (match)
import Routing.Hash (getHash, matches, setHash)
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
    route' <- awaitRoute <|>  pageForRoute' route
    go awaitRoute route'


pageForRoute'
  :: forall r
   . Routes
  -> MyApp {user :: Ref (Maybe User) | r} Routes
pageForRoute' route = do
  event <- header route <|> pageForRoute route <|> footer
  V.match
    { changeRoute : \route' -> do
        let path = (toPath route')
        log path
        liftEffect $ setHash  path
        pure route'
    }
    event


pageForRoute
  :: forall v r
   . Routes
  -> MyApp {user :: Ref (Maybe User) | r} (Variant (changeRoute :: Routes | v))
pageForRoute HomePage = homePage
pageForRoute (Profile username) = profilePage username
pageForRoute (Article slug) = articlePage slug
pageForRoute LogIn = do
  logIn
  homePage


main :: Effect Unit
main = do
  runWidgetInDom "root" routingWidget
