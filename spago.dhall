{-
Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html
-}
{ name = "insect"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "control"
  , "decimals"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "nonempty"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "quantities"
  , "strings"
  , "test-unit"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
