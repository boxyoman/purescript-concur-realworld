{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "my-project"
, dependencies =
    [ "concur-react"
    , "console"
    , "effect"
    , "profunctor-lenses"
    , "variant"
    , "milkis"
    , "simple-json"
    , "generics-rep"
    , "formatters"
    , "simple-json-generics"
    , "routing"
    ]
, packages =
    ./packages.dhall
}
