module Api where

import Prelude

import Control.Monad.Reader (class MonadReader, asks)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Milkis as M
import Types (Article, Author, Slug, Tag, Username, User)
import Utils.Api (WebRequest_, get, readAsJSON')

rootUrl :: String
rootUrl = "https://conduit.productionready.io/api"


getArticles
  :: forall m r
   . MonadAff m
  => MonadReader {user :: Ref (Maybe User) | r} m
  => m (WebRequest_ ({articles :: Array Article}))
getArticles = do
  readAsJSON' <$> get (M.URL (rootUrl <> "/articles"))


getArticle
  :: forall m r
   . MonadAff m
  => MonadReader {user :: Ref (Maybe User) | r} m
  => Slug
  -> m (WebRequest_ ({article :: Article}))
getArticle slug = do
  readAsJSON' <$> get (M.URL (rootUrl <> "/articles/" <> unwrap slug))


getTags
  :: forall m r
   . MonadAff m
  => MonadReader {user :: Ref (Maybe User) | r} m
  => m (WebRequest_ ({tags :: Array Tag}))
getTags = do
  readAsJSON' <$> get (M.URL (rootUrl <> "/tags"))


getProfile
  :: forall m r
   . MonadAff m
  => MonadReader {user :: Ref (Maybe User) | r} m
  => Username
  -> m (WebRequest_ ({profile :: Author}))
getProfile username = do
  user <- asks (_.user) >>= (liftEffect <<< Ref.read)
  let mtoken = map _.token user

  readAsJSON' <$> get (M.URL (rootUrl <> "/profiles/" <> unwrap username))


getArticlesBy
  :: forall m r
   . MonadAff m
  => MonadReader {user :: Ref (Maybe User) | r} m
  => Username
  -> m (WebRequest_ ({articles :: Array Article}))
getArticlesBy username = do
  readAsJSON' <$> get (M.URL (rootUrl <> "/articles?author=" <> unwrap username))


getArticlesFavBy
  :: forall m r
   . MonadAff m
  => MonadReader {user :: Ref (Maybe User) | r} m
  => Username
  -> m (WebRequest_ ({articles :: Array Article}))
getArticlesFavBy username = do
  readAsJSON' <$> get (M.URL (rootUrl <> "/articles?favorited=" <> unwrap username))
