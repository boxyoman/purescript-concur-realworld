module Page.NewArticle where

import Prelude

import Control.Alt ((<|>))
import Api (createArticle, getArticle)
import Concur.Core.Patterns (loopState)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Monad.Reader (asks)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Variant (Variant)
import Data.Variant as V
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Routes as R
import Types (MyApp, User, Article, mkTitle, Slug)
import React.Ref as RRef
import Unsafe.Coerce (unsafeCoerce)
import Data.Either (Either(..))
import Widgets.InputError (showErrors)


newArticlePage
  :: forall v r
   . Maybe Slug
  -> MyApp {user :: Ref (Maybe User) | r}  (Variant (changeRoute :: R.Routes | v))
newArticlePage Nothing = settingsPage Nothing
newArticlePage (Just slug) = do
  result <- getArticle slug <|> D.text "loading..."
  V.match
    { success : \article -> settingsPage (Just article.article)
    , error : \ err -> D.text (show err)
    }
    result




settingsPage
  :: forall v r
   . Maybe Article
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
settingsPage mArticle = do
  muser <- asks (_.user) >>= (liftEffect <<< Ref.read)
  case muser of
    Nothing -> D.text "You're not signed in. You're not supposed to be here"
    Just _ -> do
      D.div
        [ P.className "editor-page" ]
        [ D.div
          [ P.className "container page" ]
          [ D.div
            [ P.className "row" ]
            [ D.div
              [ P.className "col-md-10 offset-md-1 col-xs-12" ]
              [ articleForm mArticle ]
            ]
          ]
        ]


articleForm
  :: forall v r
   . Maybe Article
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
articleForm article =
  let initialState = { title : maybe "" (unwrap <<< _.title) article
                     , description : maybe "" _.description article
                     , body : maybe "" _.body article
                     , errors : []
                     }
   in loopState initialState \ s -> do
        refTitle <- liftEffect RRef.createNodeRef
        refDescription <- liftEffect RRef.createNodeRef
        refBody <- liftEffect RRef.createNodeRef

        _ <-
          D.form
            []
            [ D.fieldset
              []
              [ D.fieldset
                [ P.className "form-group" ]
                [ D.input
                  [ P.className "form-control form-control-lg"
                  , P.typeof "text"
                  , P.placeholder "Article Title"
                  , P.ref (RRef.fromRef refTitle)
                  , P.defaultValue s.title
                  ]
                ]
              , D.fieldset
                [ P.className "form-group" ]
                [ D.input
                  [ P.className "form-control"
                  , P.typeof "text"
                  , P.placeholder "What's this article about?"
                  , P.ref (RRef.fromRef refDescription)
                  , P.defaultValue s.description
                  ]
                ]
              , D.fieldset
                [ P.className "form-group" ]
                [ D.textarea
                  [ P.className "form-control"
                  , P.rows 8
                  , P.placeholder "Write your article (in markdown)"
                  , P.ref (RRef.fromRef refBody)
                  , P.defaultValue s.body
                  ]
                  []
                ]
              , D.fieldset
                [ P.className "form-group" ]
                [ D.input
                  [ P.className "form-control"
                  , P.typeof "text"
                  , P.placeholder "Enter tags"
                  , P.ref (RRef.fromRef refDescription)
                  , P.defaultValue s.description
                  ]
                , D.div
                  [ P.className "tag-list"]
                  []
                ]
              , D.button
                [ P.className "btn btn-lg pull-xs-right btn-primary"
                , P.onClick $> unit
                ]
                [ D.text "Update Settings" ]
              , showErrors s.errors
              ]
            ]

        title <- readRef refTitle s.title
        description <- readRef refDescription s.description
        body <- readRef refBody s.body
        let crArticle = { title : mkTitle title
                        , description : description
                        , body : body
                        , tagList : []
                        }
        result <- createArticle crArticle <|> D.text "Creating..."
        mErrors <-
          V.match
            { success : \ resBody -> do
                pure []
            , error : \ err -> do
               pure $ [(show err)]
            }
            result
        pure $ Left { title
                    , description
                    , body
                    , errors : mErrors
                    }
  where
    readRef ref defValue =
      liftEffect (RRef.getCurrentRef ref)
      <#> (maybe defValue (_.value <<< unsafeCoerce))

