module Types
  ( Username'
  , Username
  , mkUsername
  , Author
  , Slug'
  , Slug
  , mkSlug
  , Title'
  , Title
  , Article
  , Tag'
  , Tag
  ) where

import Data.Maybe (Maybe)
import Data.Wrapped (Wrapped(..))
import Types.DateTimeJSON (MyDateTime)


data Username'
type Username = Wrapped Username' String


mkUsername :: String -> Username
mkUsername = Wrapped


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
