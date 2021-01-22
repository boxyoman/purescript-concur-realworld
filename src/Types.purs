module Types
  ( Username'
  , Username
  , mkUsername
  , User
  , Author
  , UpdateProfile
  , Slug'
  , Slug
  , mkSlug
  , Title'
  , Title
  , mkTitle
  , CreateArticle
  , Article
  , CommentId'
  , CommentId
  , Comment
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
  , bio :: Maybe String
  , image :: Maybe String
  }

type Author =
  { username ::  Username
  , bio :: Maybe String
  , image :: String
  , following :: Boolean
  }

type UpdateProfile =
  { username ::  Maybe Username
  , email :: Maybe String
  , bio :: Maybe String
  , image :: Maybe String
  , password :: Maybe String
  }


data Slug'
type Slug = Wrapped Slug' String

mkSlug :: String -> Slug
mkSlug = Wrapped


data Title'
type Title = Wrapped Title' String

mkTitle :: String -> Title
mkTitle = Wrapped

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


type CreateArticle =
  { title :: Title
  , description :: String -- too lazy to newtype this
  , body :: String
  , tagList :: Array Tag
  }


data Tag'
type Tag = Wrapped Tag' String


data CommentId'
type CommentId = Wrapped CommentId' Int

type Comment =
  { id :: CommentId
  , createdAt :: MyDateTime
  , updatedAt :: MyDateTime
  , body :: String
  , author :: Author
  }



type MyApp r = ReaderT r (Widget HTML)
