{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ backend =
    "pskt"
, name =
    "my-project"
, dependencies =
    [ "arrays"
    , "catenable-lists"
    , "console"
    , "debug"
    , "effect"
    , "free"
    , "psci-support"
    , "record"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
