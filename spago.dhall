{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "postgresql-client"
, license = "BSD-3-Clause"
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
, repository = "https://github.com/rightfold/purescript-postgresql-client.git"
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
