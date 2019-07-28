{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "arrays"
    , "assert"
    , "bifunctors"
    , "bytestrings"
    , "console"
    , "datetime"
    , "decimals"
    , "effect"
    , "either"
    , "exceptions"
    , "foldable-traversable"
    , "foreign"
    , "foreign-generic"
    , "foreign-object"
    , "js-date"
    , "lists"
    , "maybe"
    , "newtype"
    , "nullable"
    , "prelude"
    , "psci-support"
    , "test-unit"
    , "transformers"
    , "tuples"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
