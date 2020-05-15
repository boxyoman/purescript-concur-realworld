{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "my-project"
, dependencies =
  [ "concur-react"
  , "console"
  , "effect"
  , "formatters"
  , "generics-rep"
  , "milkis"
  , "profunctor-lenses"
  , "react"
  , "refs"
  , "routing"
  , "simple-json"
  , "simple-json-generics"
  , "variant"
  ]
, packages = ./packages.dhall
}
