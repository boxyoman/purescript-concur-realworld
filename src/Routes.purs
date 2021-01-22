module Routes where

import Prelude

import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Data.Variant as V
import Routing.Match (Match, end, root, lit, str)
import Types (Slug, Username, mkSlug, mkUsername)



data Routes
  = HomePage
  | Profile Username
  | Article Slug
  | Settings
  | LogIn
  | EditArticle Slug
  | NewArticle

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
isSettings Settings = true
isSettings _ = false

isNewPost :: Routes -> Boolean
isNewPost _ = false



routes :: Match Routes
routes = root *> oneOf
  [ HomePage <$ (lit "" *> end)
  , Profile <$> (mkUsername <$> (lit "profile" *> str)) <* end
  , Article <$> (mkSlug <$> (lit "article" *> str)) <* end
  , Settings <$ (lit "settings" *> end)
  , LogIn <$ lit "login" <* end
  , EditArticle <$> (mkSlug <$> (lit "editor" *> str)) <* end
  , NewArticle <$ lit "editor" <* end
  ]


toPath :: Routes -> String
toPath HomePage = "/"
toPath (Profile username) = "/profile/" <> unwrap username
toPath (Article slug) = "/article/" <> unwrap slug
toPath Settings = "/settings"
toPath LogIn = "/login/"
toPath (EditArticle slug) = "/editor/" <> unwrap slug
toPath NewArticle = "/editor/"


changeRoute :: forall v . Routes -> Variant (changeRoute :: Routes | v)
changeRoute r = V.inj (SProxy :: SProxy "changeRoute") r


