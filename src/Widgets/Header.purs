module Widgets.Header where

import Prelude

import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alternative (empty)
import Control.Monad.Reader.Class (asks)
import Data.Maybe (Maybe, isJust)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Routes (Routes)
import Routes as Routes
import Types (MyApp, User)


header
  :: forall a  r
   . Routes
  -> MyApp { user :: Ref (Maybe User) | r} a
header page = do
  muser <- asks (_.user) >>= (liftEffect <<< Ref.read)
  D.nav
    [ P.className "navbar navbar-light" ]
    [ D.div
      [ P.className "container" ]
      [ D.a
        [ P.className "navbar-brand"
        , P.href "#/"
        ]
        [ D.text "conduit" ]
      , D.ul
        [ P.className "nav navbar-nav pull-xs-right" ]
        [ D.li
          [ P.className "nav-item" ]
          [ D.a
            [ P.className ("nav-link" <> activeRoute page Routes.isHomePage)
            , P.href "#/"
            ]
            [ D.text "Home" ]
          ]
        , newPostButton page
        , settingsButton page
        , signupButton page
        ]
      ]
    ]


signupButton
  :: forall a  r
   . Routes
  -> MyApp { user :: Ref (Maybe User) | r} a
signupButton page = do
  muser <- asks (_.user) >>= (liftEffect <<< Ref.read)
  if isJust muser
    then empty
    else
      D.li
        [ P.className "nav-item" ]
        [ D.a
          [ P.className ("nav-link" <> activeRoute page Routes.isSignUp)
          , P.href "#/signup"
          ]
          [ D.text "Sign up"
          ]
        ]


settingsButton
  :: forall a  r
   . Routes
  -> MyApp { user :: Ref (Maybe User) | r} a
settingsButton page = do
  muser <- asks (_.user) >>= (liftEffect <<< Ref.read)
  if isJust muser
    then
      D.li
        [ P.className "nav-item" ]
        [ D.a
          [ P.className ("nav-link" <> activeRoute page Routes.isSettings)
          , P.href "#/settings"
          ]
          [ D.i
            [ P.className "ion-gear-a" ]
            []
          , D.text " Settings"
          ]
        ]
    else empty


newPostButton
  :: forall a  r
   . Routes
  -> MyApp { user :: Ref (Maybe User) | r} a
newPostButton page = do
  muser <- asks (_.user) >>= (liftEffect <<< Ref.read)
  if isJust muser
    then
      D.li
        [ P.className "nav-item" ]
        [ D.a
          [ P.className ("nav-link" <> activeRoute page Routes.isNewPost)
          , P.href "#/newpost"
          ]
          [ D.i
            [ P.className "ion-compose" ]
            []
          , D.text " New Post"
          ]
        ]
    else empty


activeRoute :: forall b. b -> (b -> Boolean) -> String
activeRoute route pred =
  if pred route
    then " active"
    else ""


footer
  :: forall a  r
   . MyApp { user :: Ref (Maybe User) | r} a
footer =
  D.footer
    []
    [ D.div
      [ P.className "container" ]
      [ D.a
        [ P.className "logo-font"
        , P.href "#/"
        ]
        [ D.text "conduit"
        ]
      , D.span
        [ P.className "attribution" ]
        [ D.text "An interactive learning project from "
        , D.a
          [ P.href "https://thinkster.io"
          ]
          [ D.text "Thinkster"
          ]
       , D.text " Code & design licensed under MIT."
       ]
      ]
    ]
