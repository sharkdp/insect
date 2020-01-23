let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.5-20200103/packages.dhall sha256:0a6051982fb4eedb72fbe5ca4282259719b7b9b525a4dda60367f98079132f30

in      upstream
    //  { quantities =
            { dependencies =
              [ "lists"
              , "foldable-traversable"
              , "numbers"
              , "pairs"
              , "decimals"
              ]
            , repo = "https://github.com/sharkdp/purescript-quantities.git"
            , version = "v10.0.0"
            }
        }
