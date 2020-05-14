module Page.Profile where

import Prelude

import Api (getArticlesBy, getArticlesFavBy, getProfile)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Monad.Rec.Class (forever)
import Control.MultiAlternative (orr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.RemoteData as RD
import Data.Variant as V
import Effect.Ref (Ref)
import Page.ArticlePreview (getAndViewArticles)
import Types (Author, MyApp, Username, User)

data PageArticle = Faved | WrittenBy

derive instance genericPageArticle :: Generic PageArticle _
instance eqPageArticle :: Eq PageArticle where
  eq = genericEq
instance showPageArticle :: Show PageArticle where
  show = genericShow


profilePage :: forall a r . Username -> MyApp { user :: Ref (Maybe User) |r } a
profilePage username =
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
          [profileArticles username WrittenBy]
        ]
      ]
    ]


profileArticles
  :: forall a r
   . Username
  -> PageArticle
  -> MyApp { user :: Ref (Maybe User) |r} a
profileArticles username pageArticle = do
  pageArticle' <- orr $
    [ D.div
      [ P.className "articles-toggle" ]
      [ D.ul
        [ P.className "nav nav-pills outline-active" ]
        [ D.li
          [ P.className "nav-item" ]
          [ D.a
            [ P.className ("nav-link" <> activeCss pageArticle WrittenBy)
            , P.onClick $> WrittenBy
            ]
            [D.text "My Articles"]
          ]
        , D.li
          [ P.className "nav-item" ]
          [ D.a
            [ P.className ("nav-link" <> activeCss pageArticle Faved)
            , P.onClick $> Faved
            ]
            [D.text "Favorited Articles"]
          ]
        ]
      ]
    , case pageArticle of
        WrittenBy -> getAndViewArticles (getArticlesBy username)
        Faved -> getAndViewArticles (getArticlesFavBy username)
    ]
  profileArticles username pageArticle'

  where
    activeCss :: forall eq . Eq eq => eq -> eq -> String
    activeCss current wanted =
      if current == wanted
        then " active"
        else ""

profileInfo :: forall a r . Username -> MyApp { user :: Ref (Maybe User) |r } a
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


profileInfoView :: forall a r . Author -> MyApp { user :: Ref (Maybe User) |r } a
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

