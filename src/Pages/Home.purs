module Page.Home where

import Prelude

import Api (getArticles, getTags)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Control.MultiAlternative (orr)
import Data.Functor (mapFlipped)
import Data.Newtype (unwrap)
import Data.RemoteData as RD
import Data.Variant as V
import Types (Article)


homePage :: forall a. Widget HTML a
homePage = forever do
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
          [ D.div
            [ P.className "feed-toggle" ]
            [ D.ul
              [ P.className "nav nav-pills outline-active" ]
              [ D.li
                [ P.className "nav-item" ]
                [ D.a
                  [ P.className "nav-link disabled"
                  , P.href ""
                  ]
                  [D.text "Your Feed"]
                ]
              , D.li
                [ P.className "nav-item" ]
                [ D.a
                  [ P.className "nav-link active"
                  , P.href ""
                  ]
                  [D.text "Global Feed"]
                ]
              ]
            ]
          , globalFeed
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


tagsView :: forall a. Widget HTML a
tagsView = do
  rdTags <- getTags <|> D.text "loading"
  V.case_
    # V.on RD._success (\tags ->
        forever $ orr $ mapFlipped tags.tags $ \tag ->
          D.a
            [ P.href ""
            , P.className "tag-pill tag-default"
            ]
            [ D.text (unwrap tag)
            ]
      )
    # V.on RD._error (\err ->
        forever $ D.text (show err)
      )
    $ rdTags




globalFeed :: forall a. Widget HTML a
globalFeed =  do
  remoteArticles <- getArticles <|> D.text "loading"
  V.case_
    # V.on RD._success (\articles ->
        forever $ orr $ map articleView articles.articles
      )
    # V.on RD._error (\err ->
        forever $ D.text (show err)
      )
    $ remoteArticles


articleView :: forall a . Article -> Widget HTML a
articleView article =
  D.div
    [P.className "article-preview"]
    [ D.div
      [P.className "article-meta" ]
      [ D.a
        [P.href authorProfileUrl]
        [D.img [P.src article.author.image]]
      , D.div
        [ P.className "info" ]
        [ D.a
          [ P.href authorProfileUrl
          , P.className "author"
          ]
          [ D.text $ unwrap article.author.username ]
        , D.span [P.className "date"] [D.text (show article.createdAt)]
        ]
      , D.button
        [ P.className "btn btn-outline-primary btn-sm pull-xs-right" ]
        [ D.i [P.className "ion-heart"] []
        , D.text (" " <> show article.favoritesCount)
        ]
      ]
    , D.a
      [ P.href ("#/article/" <> unwrap article.slug)
      , P.className "preview-link"
      ]
      [ D.h1' [D.text $ unwrap article.title]
      , D.p' [D.text article.description]
      , D.span' [D.text "Read more..."]
      ]
    ]
  where
    authorProfileUrl = "#/profile/" <> unwrap article.author.username
