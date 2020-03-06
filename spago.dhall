{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "purescript-concur-starter"
, dependencies =
    [ "concur-core"
    , "concur-react"
    , "console"
    , "datetime"
    , "effect"
    , "formatters"
    , "now"
    ]
, packages = ./packages.dhall
}
