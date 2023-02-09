{-
Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html
-}
{ name = "insect"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "control"
  , "decimals"
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
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
