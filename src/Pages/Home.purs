module Page.Home where

import Prelude

import Api (getArticles, getFeed, getTags)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Monad.Reader (asks)
import Control.MultiAlternative (orr)
import Data.Functor (mapFlipped)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, isJust)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Data.Variant as V
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Routes (Routes)
import Types (MyApp, User)
import Widgets.ArticlePreview (getAndViewArticles)


homePage
  :: forall r v
   . MyApp { user :: Ref (Maybe User) |r } (Variant (changeRoute :: Routes | v))
homePage = do
  D.div
    [ P.className "home-page" ]
    [ D.div
      [ P.className "banner" ]
      [ D.div
        [ P.className "container" ]
        [ D.h1
          [P.className "logo-font"]
          [D.text "Conduit"]
        , D.p' [D.text "A place to share your knowledge."]
        ]
      ]
    , D.div
      [ P.className "container page" ]
      [ D.div
        [ P.className "row" ]
        [ D.div
          [ P.className "col-md-9" ]
          [ articlesView Global
          ]
        , D.div
          [ P.className "col-md-3" ]
          [ D.div
            [ P.className "sidebar" ]
            [ D.p' [D.text "Popular Tags" ]
            , tagsView
            ]
          ]
        ]
      ]
    ]


data ArticleSettings = Global | YourFeed

derive instance genericArticleSettings :: Generic ArticleSettings _
instance eqArticleSettings :: Eq ArticleSettings where
  eq = genericEq
instance showArticleSettings :: Show ArticleSettings where
  show = genericShow


_pageFeed = (SProxy :: SProxy "pageFeed")

changePageFeed
  :: forall v. ArticleSettings -> (Variant (pageFeed :: ArticleSettings | v))
changePageFeed =
  V.inj _pageFeed


articlesView
  :: forall v r
   . ArticleSettings
  -> MyApp { user :: Ref (Maybe User) | r } (Variant (changeRoute :: Routes | v))
articlesView artSettings = do
  user <- asks (_.user) >>= (liftEffect <<< Ref.read)
  artSettings' <- orr $
    [ D.div
      [ P.className "feed-toggle" ]
      [ D.ul
        [ P.className "nav nav-pills outline-active" ]
        [ if isJust user
            then D.li
                  [ P.className "nav-item" ]
                  [ D.a
                    [ P.className ("nav-link " <> activeCss artSettings YourFeed)
                    , P.onClick $> changePageFeed YourFeed
                    ]
                    [D.text "Your Feed"]
                  ]
            else empty
        , D.li
          [ P.className "nav-item" ]
          [ D.a
            [ P.className ("nav-link " <> activeCss artSettings Global)
            , P.onClick $> changePageFeed Global
            ]
            [D.text "Global Feed"]
          ]
        ]
      ]
    , case artSettings of
        Global -> getAndViewArticles getArticles
        YourFeed -> getAndViewArticles getFeed
    ]
  V.on _pageFeed (articlesView) pure artSettings'

  where
    activeCss :: forall eq . Eq eq => eq -> eq -> String
    activeCss current wanted =
      if current == wanted
        then " active"
        else ""


tagsView :: forall a r. MyApp { user :: Ref (Maybe User) |r } a
tagsView = do
  rdTags <- getTags <|> D.text "loading"
  V.match
    { success : \tags ->
        orr $ mapFlipped tags.tags $ \tag ->
          D.a
            [ P.href ""
            , P.className "tag-pill tag-default"
            ]
            [ D.text (unwrap tag)
            ]
    , error : \err ->
        D.text (show err)
    }
    rdTags
