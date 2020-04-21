module Page.Profile where

import Prelude

import Api (getProfile, getArticlesBy)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.RemoteData as RD
import Data.Variant as V
import Page.ArticlePreview (getAndViewArticles)
import Types (Username, Author)


profilePage :: forall a . Username -> Widget HTML a
profilePage username = do
  D.div
    [ P.className "profile-page" ]
    [ D.div
      [ P.className "user-info" ]
      [ profileInfo username
      ]
    , D.div
      [ P.className "container" ]
      [ D.div
        [ P.className "row" ]
        [ D.div
          [ P.className "col-xs-12 col-md-10 offset-md-1" ]
          [ D.div
            [ P.className "articles-toggle" ]
            [ D.ul
              [ P.className "nav nav-pills outline-active" ]
              [ D.li
                [ P.className "nav-item" ]
                [ D.a
                  [ P.className "nav-link active"
                  ]
                  [D.text "My Articles"]
                ]
              , D.li
                [ P.className "nav-item" ]
                [ D.a
                  [ P.className "nav-link"
                  ]
                  [D.text "Favorited Articles"]
                ]
              ]
            ]
          , getAndViewArticles (getArticlesBy username)
          ]
        ]
      ]
    ]

profileInfo :: forall a . Username -> Widget HTML a
profileInfo username = do
  rdProfile <- getProfile username <|> D.text "loading"
  V.case_
    # V.on RD._success (\profile ->
        profileInfoView profile.profile
      )
    # V.on RD._error (\err ->
        forever $ D.text (show err)
      )
    $ rdProfile


profileInfoView :: forall a . Author -> Widget HTML a
profileInfoView author =
  D.div
    [ P.className "row" ]
    [ D.div
      [ P.className "col-xs-12 col-md-10 offset-md-1" ]
      [ D.img
        [ P.src author.image
        , P.className "user-img"
        ]
      , D.h4' [ D.text $ unwrap author.username ]
      , case author.bio of
          Just bio -> D.p' [ D.text bio ]
          Nothing -> empty
      , D.button
        [ P.className "btn btn-sm btn-outline-secondary action-btn"
        ]
        [ D.i [P.className "ion-plus-round"] []
        , D.text ("  Follow " <> unwrap author.username)
        ]
      ]
    ]

