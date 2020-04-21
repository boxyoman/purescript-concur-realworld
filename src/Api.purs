module Api where

import Prelude

import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Milkis as M
import Types (Article, Author, Tag, Username, Slug)
import Utils.Api (WebRequest_, get, readAsJSON')

rootUrl :: String
rootUrl = "https://conduit.productionready.io/api"


getArticles
  :: forall m
   . MonadAff m
  => m (WebRequest_ ({articles :: Array Article}))
getArticles = do
  readAsJSON' <$> get (M.URL (rootUrl <> "/articles"))


getArticle
  :: forall m
   . MonadAff m
  => Slug
  -> m (WebRequest_ ({article :: Article}))
getArticle slug = do
  readAsJSON' <$> get (M.URL (rootUrl <> "/articles/" <> unwrap slug))


getTags
  :: forall m
   . MonadAff m
  => m (WebRequest_ ({tags :: Array Tag}))
getTags = do
  readAsJSON' <$> get (M.URL (rootUrl <> "/tags"))


getProfile
  :: forall m
   . MonadAff m
  => Username
  -> m (WebRequest_ ({profile :: Author}))
getProfile username = do
  readAsJSON' <$> get (M.URL (rootUrl <> "/profiles/" <> unwrap username))


getArticlesBy
  :: forall m
   . MonadAff m
  => Username
  -> m (WebRequest_ ({articles :: Array Article}))
getArticlesBy username = do
  readAsJSON' <$> get (M.URL (rootUrl <> "/articles?author=" <> unwrap username))
