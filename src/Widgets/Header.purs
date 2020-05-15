module Widgets.Header where

import Prelude

import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alternative (empty)
import Control.Monad.Reader.Class (asks)
import Data.Maybe (Maybe, isJust)
import Data.Variant (Variant)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Routes (Routes)
import Routes as R
import Types (MyApp, User)


header
  :: forall v r
   . Routes
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: Routes | v))
header page = do
  muser <- asks (_.user) >>= (liftEffect <<< Ref.read)
  D.nav
    [ P.className "navbar navbar-light" ]
    [ D.div
      [ P.className "container" ]
      [ D.a
        [ P.className "navbar-brand"
        , P.onClick $> (R.changeRoute R.HomePage)
        ]
        [ D.text "conduit" ]
      , D.ul
        [ P.className "nav navbar-nav pull-xs-right" ]
        [ D.li
          [ P.className "nav-item" ]
          [ D.a
            [ P.className ("nav-link" <> activeRoute page R.isHomePage)
            , P.onClick $> (R.changeRoute R.HomePage)
            ]
            [ D.text "Home" ]
          ]
        , newPostButton page
        , settingsButton page
        , signinButton page
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
          [ P.className ("nav-link" <> activeRoute page R.isSignUp)
          , P.href "#/signup"
          ]
          [ D.text "Sign up"
          ]
        ]

signinButton
  :: forall v r
   . Routes
  -> MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: Routes | v))
signinButton page = do
  muser <- asks (_.user) >>= (liftEffect <<< Ref.read)
  if isJust muser
    then empty
    else
      D.li
        [ P.className "nav-item" ]
        [ D.a
          [ P.className ("nav-link" <> activeRoute page R.isLogIn)
          , P.onClick $> (R.changeRoute R.LogIn)
          ]
          [ D.text "Sign in"
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
          [ P.className ("nav-link" <> activeRoute page R.isSettings)
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
          [ P.className ("nav-link" <> activeRoute page R.isNewPost)
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
  :: forall v  r
   . MyApp { user :: Ref (Maybe User) | r} (Variant (changeRoute :: Routes | v))
footer =
  D.footer
    []
    [ D.div
      [ P.className "container" ]
      [ D.a
        [ P.className "logo-font"
        , P.onClick $> (R.changeRoute R.HomePage)
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
