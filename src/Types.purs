module Types
  ( Username'
  , Username
  , mkUsername
  , User
  , Author
  , Slug'
  , Slug
  , mkSlug
  , Title'
  , Title
  , Article
  , Tag'
  , Tag
  , MyApp
  ) where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Maybe (Maybe)
import Data.Wrapped (Wrapped(..))
import Types.DateTimeJSON (MyDateTime)


data Username'
type Username = Wrapped Username' String


mkUsername :: String -> Username
mkUsername = Wrapped

type User =
  { username :: Username
  , email :: String
  , token :: String
  , bio :: String
  , image :: Maybe String
  }

type Author =
  { username ::  Username
  , bio :: Maybe String
  , image :: String
  , following :: Boolean
  }

data Slug'
type Slug = Wrapped Slug' String

mkSlug :: String -> Slug
mkSlug = Wrapped


data Title'
type Title = Wrapped Title' String

type Article =
  { slug :: Slug
  , title :: Title
  , description :: String -- too lazy to newtype this
  , body :: String
  , createdAt ::  MyDateTime
  , updatedAt ::  MyDateTime
  , favorited :: Boolean
  , favoritesCount :: Int
  , author :: Author
  , tagList :: Array Tag
  }


data Tag'
type Tag = Wrapped Tag' String


type MyApp r = ReaderT r (Widget HTML)
