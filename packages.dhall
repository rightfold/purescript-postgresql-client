let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210506/packages.dhall sha256:d199e142515f9cc15838d8e6d724a98cd0ca776ceb426b7b36e841311643e3ef

in  upstream
      with polyform =
        { dependencies =
          [ "arrays"
          , "bifunctors"
          , "control"
          , "effect"
          , "either"
          , "enums"
          , "functors"
          , "identity"
          , "invariant"
          , "lists"
          , "maybe"
          , "newtype"
          , "parallel"
          , "partial"
          , "prelude"
          , "profunctor"
          , "psci-support"
          , "quickcheck"
          , "quickcheck-laws"
          , "record"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          , "validation"
          , "variant"
          ]
        , repo = "https://github.com/purescript-polyform/polyform.git"
        , version = "v0.9.0"
        }
      with polyform-batteries-core =
        { dependencies =
          [ "arrays"
          , "decimals"
          , "effect"
          , "enums"
          , "integers"
          , "lazy"
          , "maybe"
          , "numbers"
          , "partial"
          , "polyform"
          , "prelude"
          , "psci-support"
          , "quickcheck"
          , "strings"
          , "test-unit"
          , "typelevel-prelude"
          , "validation"
          , "variant"
          ]
        , repo = "https://github.com/purescript-polyform/batteries-core.git"
        , version = "v0.2.0"
        }
      with polyform-batteries-env =
        { dependencies =
          [ "arrays"
          , "identity"
          , "maybe"
          , "ordered-collections"
          , "polyform"
          , "polyform-batteries-core"
          , "prelude"
          , "psci-support"
          , "typelevel-prelude"
          ]
        , repo = "https://github.com/purescript-polyform/batteries-env.git"
        , version = "v0.1.0"
        }
