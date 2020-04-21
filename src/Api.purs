module Api
  ( getArticles
  , getTags
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Milkis as M
import Types (Article, Tag)
import Utils.Api (WebRequest_, get, readAsJSON')

rootUrl :: String
rootUrl = "https://conduit.productionready.io/api"


getArticles
  :: forall m
   . MonadAff m
  => m (WebRequest_ ({articles :: Array Article}))
getArticles = do
  readAsJSON' <$> get (M.URL (rootUrl <> "/articles"))


getTags
  :: forall m
   . MonadAff m
  => m (WebRequest_ ({tags :: Array Tag}))
getTags = do
  readAsJSON' <$> get (M.URL (rootUrl <> "/tags"))
