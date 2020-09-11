module Page.Article where

import Prelude

import Api (getArticle, getCommentsForArticle)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Monad.Reader (asks)
import Control.MultiAlternative (orr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Variant (Variant)
import Data.Variant as V
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Routes as R
import Types (Article, MyApp, Slug, User, Comment)
import Types.DateTimeJSON (viewDate)


articlePage
  :: forall v r
   . Slug
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
articlePage slug = do
  rdArticle <- getArticle slug <|> D.text "loading"
  V.match
    { success : \article -> articleView article.article
    , error : \err -> D.text (show err)
    }
    rdArticle


articleView
  :: forall v r
   . Article
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
articleView article =
  D.div
    [ P.className "article-page" ]
    [ D.div
      [ P.className "banner" ]
      [ D.div
        [ P.className "container" ]
        [ D.h1' [ D.text (unwrap article.title) ]
        , articleMetaView article
        ]
      ]
    , D.div
      [ P.className "container page" ]
      [ D.div
        [ P.className "row article-content" ]
        [ D.div
          [ P.className "col-md-12" ]
          [ D.text article.body ] -- TODO figure out how to markdown
        ]
      , D.hr'
      , D.div
        [ P.className "article-actions" ]
        [ articleMetaView article ]
      , D.div
        [ P.className "row" ]
        [ D.div
          [ P.className "col-xs-13 col-md-8 offset-md-2" ]
          [comments article]
        ]
      ]
    ]


comments
  ::  forall v r
   . Article
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
comments article =
  postComments article <|>  showComments article


postComments
  ::  forall v r
   . Article
  -> MyApp { user :: Ref (Maybe User) | r} (Variant v)
postComments article = do
  muser <- asks (_.user) >>= (liftEffect <<< Ref.read)
  case muser of
    Just user ->
      let imgSrc =
            fromMaybe
              "https://static.productionready.io/images/smiley-cyrus.jpg"
              user.image
       in
        D.form
          [ P.className "card comment-form" ]
          [ D.div
            [ P.className "card-block" ]
            [ D.textarea
              [ P.className "form-control"
              , P.placeholder "Write a comment..."
              , P.rows 3
              ]
              []
            ]
          , D.div
            [ P.className "card-block" ]
            [ D.img
              [ P.src imgSrc
              , P.alt (unwrap user.username)
              , P.className "comment-author-img"
              ]
            , D.button
              [ P.className "btn btn-sm btn-primary"
              ]
              [ D.text "Post Comment"
              ]
            ]
          ]

    Nothing ->
      D.text ""

showComments
  ::  forall v r
   . Article
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
showComments article = do
  rdComments <- getCommentsForArticle article.slug <|> D.text "loading"
  V.match
    { success : \comm -> orr $ map showComment comm.comments
    , error : \err -> D.text (show err)
    }
    rdComments




showComment
  ::  forall v r
   . Comment
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
showComment comment =
  D.div
    [ P.className "card" ]
    [ D.div
      [ P.className "card-block" ]
      [ D.p
        [ P.className "card-text" ]
        [ D.text comment.body ]
      ]
    , D.div
      [ P.className "card-footer" ]
      [ D.a
        [ P.className "comment-author"
        , P.onClick $> R.changeRoute (R.Profile comment.author.username)
        ]
        [ D.img
          [ P.href comment.author.image
          , P.className "comment-author-img"
          ]
        ]
      , D.a
        [ P.className "comment-author"
        , P.onClick $> R.changeRoute (R.Profile comment.author.username)
        ]
        [ D.text (unwrap comment.author.username)
        ]
      , D.span
        [ P.className "date-posted" ]
        [ viewDate comment.createdAt ]
      , D.span
        [ P.className "mod-options" ]
        [ D.i [P.className "ion-edit"] [] ]
      ]
    ]



articleMetaView
  :: forall v r
   . Article
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
articleMetaView article =
  D.div
    [ P.className "article-meta" ]
    [ D.a
      [P.onClick $> R.changeRoute (R.Profile article.author.username)]
      [ D.img
        [ P.src article.author.image ]
      ]
    , D.div
      [ P.className "info" ]
      [ D.a
        [P.onClick $> R.changeRoute (R.Profile article.author.username)]
        [ D.text $ unwrap article.author.username ]
      , viewDate article.createdAt
      ]
    , empty -- TODO: follow and like here
    ]
