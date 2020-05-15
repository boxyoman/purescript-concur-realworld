module Routes where

import Prelude
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Effect.Ref (Ref)
import Page.Article (articlePage)
import Page.Home (homePage)
import Page.LogIn (logIn)
import Page.Profile (profilePage)
import Routing.Match (Match, end, root, lit, str)
import Types (MyApp, Slug, Username, User, mkSlug, mkUsername)



data Routes
  = HomePage
  | Profile Username
  | Article Slug
  | LogIn

derive instance genericRoutes :: Generic Routes _
instance showRoutes :: Show Routes where
  show = genericShow


isHomePage :: Routes -> Boolean
isHomePage HomePage = true
isHomePage _ = false

isSignUp :: Routes -> Boolean
isSignUp _ = false

isLogIn :: Routes -> Boolean
isLogIn LogIn = true
isLogIn _ = false


isSettings :: Routes -> Boolean
isSettings _ = false

isNewPost :: Routes -> Boolean
isNewPost _ = false

routes :: Match Routes
routes = root *> oneOf
  [ HomePage <$ (root *> end)
  , Profile <$> (mkUsername <$> (lit "profile" *> str)) <* end
  , Article <$> (mkSlug <$> (lit "article" *> str)) <* end
  , LogIn <$ lit "login" <* end
  ]



pageForRoute :: forall a r . Routes -> MyApp { user :: Ref (Maybe User) | r} a
pageForRoute HomePage = homePage
pageForRoute (Profile username) = profilePage username
pageForRoute (Article slug) = articlePage slug
pageForRoute LogIn = do
  logIn
  homePage
