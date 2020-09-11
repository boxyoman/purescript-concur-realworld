module Page.Settings where

import Prelude

import Control.Alt ((<|>))
import Api (updateProfile)
import Concur.Core.Patterns (loopState)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Monad.Reader (asks)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Variant (Variant)
import Data.Variant as V
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Routes as R
import Types (MyApp, User, mkUsername)
import React.Ref as RRef
import Data.Symbol (SProxy(..))
import Auth as Auth
import Unsafe.Coerce (unsafeCoerce)
import Data.Either (Either(..))
import Widgets.InputError (showErrors)


_logout = (SProxy :: SProxy "logout")


logout :: forall v . Variant (logout :: Unit | v)
logout = V.inj _logout unit


settingsPage
  :: forall v r
   . MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
settingsPage = do
  muser <- asks (_.user) >>= (liftEffect <<< Ref.read)
  case muser of
    Nothing -> D.text "You're not signed in. You're not supposed to be here"
    Just user -> do
      result <-
        D.div
          [ P.className "settings-page" ]
          [ D.div
            [ P.className "container page" ]
            [ D.div
              [ P.className "row" ]
              [ D.div
                [ P.className "col-md-6 offset-md-3 col-xs-12" ]
                [ D.h1
                  [ P.className "text-xs-center" ]
                  [ D.text "Your Settings" ]
                , settingsForm user
                , D.hr []
                , D.button
                  [ P.className "btn btn-outline-danger"
                  , P.onClick $> logout
                  ]
                  [ D.text "Or click here to logout." ]
                ]
              ]
            ]
          ]

      V.on
        _logout
        (\ _ -> do
          Auth.logout
          pure (R.changeRoute R.HomePage)
        )
        pure
        result

settingsForm
  :: forall v r
   . User
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: R.Routes | v))
settingsForm user =
  let initialState = { image : fromMaybe "" user.image
                     , name : unwrap user.username
                     , bio : fromMaybe "" user.bio
                     , email : user.email
                     , password : ""
                     , errors : []
                     }
   in loopState initialState \ s -> do
        refImg <- liftEffect RRef.createNodeRef
        refName <- liftEffect RRef.createNodeRef
        refBio <- liftEffect RRef.createNodeRef
        refEmail <- liftEffect RRef.createNodeRef
        refPassword <- liftEffect RRef.createNodeRef

        _ <-
          D.form
            []
            [ D.fieldset
              []
              [ D.fieldset
                [ P.className "form-group" ]
                [ D.input
                  [ P.className "form-control"
                  , P.typeof "text"
                  , P.placeholder "URL of profile picture"
                  , P.ref (RRef.fromRef refImg)
                  , P.defaultValue s.image
                  ]
                ]
              , D.fieldset
                [ P.className "form-group" ]
                [ D.input
                  [ P.className "form-control form-control-lg"
                  , P.typeof "text"
                  , P.placeholder "Your Name"
                  , P.ref (RRef.fromRef refName)
                  , P.defaultValue s.name
                  ]
                ]
              , D.fieldset
                [ P.className "form-group" ]
                [ D.textarea
                  [ P.className "form-control form-control-lg"
                  , P.rows 8
                  , P.placeholder "Short bio about you"
                  , P.ref (RRef.fromRef refBio)
                  , P.defaultValue s.bio
                  ]
                  []
                ]
              , D.fieldset
                [ P.className "form-group" ]
                [ D.input
                  [ P.className "form-control form-control-lg"
                  , P.typeof "text"
                  , P.placeholder "Email"
                  , P.ref (RRef.fromRef refEmail)
                  , P.defaultValue s.email
                  ]
                ]
              , D.fieldset
                [ P.className "form-group" ]
                [ D.input
                  [ P.className "form-control form-control-lg"
                  , P.typeof "password"
                  , P.placeholder "Password"
                  , P.ref (RRef.fromRef refPassword)
                  , P.defaultValue s.password
                  ]
                ]
              , D.button
                [ P.className "btn btn-lg btn-primary pull-xs-right"
                , P.onClick $> unit
                ]
                [ D.text "Update Settings" ]
              , showErrors s.errors
              ]
            ]

        image <- readRef refImg s.image
        name <- readRef refName s.name
        bio <- readRef refBio s.bio
        email <- readRef refEmail s.email
        password <- readRef refPassword s.password
        let body = { username : mkUsername <$> toMaybeVal name
                   , email : toMaybeVal email
                   , bio : toMaybeVal bio
                   , image : toMaybeVal image
                   , password : toMaybeVal password
                   }
        result <- updateProfile body <|> D.text "Updating..."
        mErrors <-
          V.match
            { success : \ resBody -> do
                Auth.updateUserRef resBody.user
                pure []
            , error : \ err -> do
               pure $ [(show err)]
            }
            result
        pure $ Left { image
                    , name
                    , bio
                    , email
                    , password
                    , errors : mErrors
                    }
  where
    toMaybeVal val =
      if val == "" then Nothing else Just val

    readRef ref defValue =
      liftEffect (RRef.getCurrentRef ref)
      <#> (maybe defValue (_.value <<< unsafeCoerce))
