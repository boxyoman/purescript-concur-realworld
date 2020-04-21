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
import Page.ArticlePreview (getAndViewArticles)
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
          , getAndViewArticles getArticles
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



