module Main where

import Prelude

import Api (getArticles)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Control.MultiAlternative (orr)
import Data.Either (either)
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.AVar as Evar
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Page.Artivle (articlePage)
import Page.Home (homePage)
import Page.Profile (profilePage)
import Routing (match)
import Routing.Hash (getHash, matches)
import Routing.Match (Match, end, int, root, lit, str)
import Types (Slug, Username, mkSlug, mkUsername)


data Routes
  = HomePage
  | Profile Username
  | Article Slug

derive instance genericRoutes :: Generic Routes _
instance showRoutes :: Show Routes where
  show = genericShow


routes :: Match Routes
routes = root *> oneOf
  [ HomePage <$ (root *> end)
  , Profile <$> (mkUsername <$> (lit "profile" *> str)) <* end
  , Article <$> (mkSlug <$> (lit "article" *> str)) <* end
  ]


routingWidget :: forall a. Widget HTML a
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
    route' <- awaitRoute <|> pageForRoute route
    go awaitRoute route'


pageForRoute :: forall a . Routes -> Widget HTML a
pageForRoute HomePage = homePage
pageForRoute (Profile username) = profilePage username
pageForRoute (Article slug) = articlePage slug


main :: Effect Unit
main = do
  runWidgetInDom "root" routingWidget
