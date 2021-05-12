let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210506/packages.dhall sha256:d199e142515f9cc15838d8e6d724a98cd0ca776ceb426b7b36e841311643e3ef

in  upstream
    with polyform =
      { dependencies =
        [ "debug", "foreign", "foreign-object", "invariant", "newtype"
        , "ordered-collections", "parsing", "psci-support", "profunctor", "quickcheck-laws"
        , "run", "test-unit", "transformers", "validation", "variant"
        ]
      , repo = "https://github.com/purescript-polyform/polyform.git"
      , version = "master"
      }
    with polyform-batteries =
      { dependencies =
        [ "affjax", "argonaut", "debug", "decimals", "filterable"
        , "numbers", "polyform", "prelude", "record-extra"
        , "test-unit"
        ]
      , repo = "https://github.com/purescript-polyform/batteries.git"
      , version = "master"
      }
    with polyform-batteries-env =
      { dependencies =
        [ "polyform-batteries" ]
      , repo = "https://github.com/purescript-polyform/batteries-env.git"
      , version = "master"
      }
