module Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (either)
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar as Evar
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Page.Article (articlePage)
import Page.Home (homePage)
import Page.Profile (profilePage)
import Routing (match)
import Routing.Hash (getHash, matches)
import Routing.Match (Match, end, root, lit, str)
import Types (MyApp, Slug, Username, User, mkSlug, mkUsername)


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
    route' <- awaitRoute <|> pageForRoute route
    go awaitRoute route'


pageForRoute :: forall a r . Routes -> MyApp { user :: Ref (Maybe User) | r} a
pageForRoute HomePage = homePage
pageForRoute (Profile username) = profilePage username
pageForRoute (Article slug) = articlePage slug


main :: Effect Unit
main = do
  user <- Ref.new Nothing
  runWidgetInDom "root" (runReaderT routingWidget {user})
