{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "postgresql-client"
, license = "BSD-3-Clause"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "assert"
  , "bifunctors"
  , "bytestrings"
  , "console"
  , "datetime"
  , "decimals"
  , "dotenv"
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
  , "node-fs"
  , "node-process"
  , "nullable"
  , "polyform-batteries-env"
  , "prelude"
  , "psci-support"
  , "string-parsers"
  , "test-unit"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, repository = "https://github.com/rightfold/purescript-postgresql-client.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
