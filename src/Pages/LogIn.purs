module Page.LogIn where

import Prelude

import Concur.Core.Patterns (loopState)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Data.RemoteData as RD
import Data.Variant as V
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import React.Ref as RRef
import Types (MyApp, User)
import Unsafe.Coerce (unsafeCoerce)
import Widgets.InputError (showErrors)
import Auth as Auth


logIn :: forall r . MyApp { user :: Ref (Maybe User) | r} Unit
logIn = loopState {msg: [], email: "", pass: ""} \s -> do
  refEmail <- liftEffect RRef.createNodeRef
  refPassword <- liftEffect RRef.createNodeRef
  _ <- D.div
      [ P.className "auth-page" ]
      [ D.div
        [ P.className "container page" ]
        [ D.div
          [ P.className "row" ]
          [ D.div
            [ P.className "col-md-6 offset-md-3 col-xs-12" ]
            [ D.h1
              [ P.className "text-xs-center" ]
              [ D.text "Sign In" ]
            , D.p
              [ P.className "text-xs-center" ]
              [ D.a
                [ P.href "#/signup"
                ]
                [ D.text "Need an account?"
                ]
              ]
            , showErrors s.msg
            , D.form
              []
              [ D.fieldset
                []
                [ D.fieldset
                  [ P.className "form-group" ]
                  [ D.input
                    [ P._type "email"
                    , P.ref (RRef.fromRef refEmail)
                    , P.onKeyEnter $> unit
                    , P.defaultValue s.email
                    , P.className "form-control form-control-lg"
                    , P.placeholder "Email"
                    ]
                  ]
                , D.fieldset
                  [ P.className "form-group" ]
                  [ D.input
                    [ P._type "password"
                    , P.ref (RRef.fromRef refPassword)
                    , P.onKeyEnter $> unit
                    , P.defaultValue s.pass
                    , P.className "form-control form-control-lg"
                    , P.placeholder "Password"
                    ]
                  ]
                , D.button
                  [ P.className "btn btn-lg btn-primary pull-xs-right"
                  , P._type "submit"
                  , P.onClick $> unit
                  ]
                  [ D.text "Sign in"]
                ]
              ]
            ]
          ]
        ]
      ]
  email <- liftEffect (RRef.getCurrentRef refEmail) <#> (maybe s.email (_.value <<< unsafeCoerce))
  password <- liftEffect (RRef.getCurrentRef refPassword) <#> (maybe s.pass (_.value <<< unsafeCoerce))
  result <- Auth.login {email, password}

  V.case_
    # V.on RD._success (\user -> do
        pure $ Right unit
      )
    # V.on RD._error (\err ->
        pure $ Left { email, pass: password, msg: [show err] }
      )
    $ result
