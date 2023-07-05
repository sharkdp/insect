{-
Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
  * How to override a package in the package set with a local one: https://github.com/purescript/spago#override-a-package-in-the-package-set-with-a-local-one
  * How to override a package in the package set with a remote one: https://github.com/purescript/spago#override-a-package-in-the-package-set-with-a-remote-one
  * How to add a package to the package set: https://github.com/purescript/spago#add-a-package-to-the-package-set
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.9-20230629/packages.dhall
        sha256:f91d36c7e4793fe4d7e042c57fef362ff3f9e9ba88454cd38686701e30bf545a

in  upstream
