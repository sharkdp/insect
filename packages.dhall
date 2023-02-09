{-
Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
  * How to override a package in the package set with a remote one: https://github.com/purescript/spago#override-a-package-in-the-package-set-with-a-remote-one
  * How to add a package to the package set: https://github.com/purescript/spago#add-a-package-to-the-package-set
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230207/packages.dhall
        sha256:c0838886820a04fe6d1332a470cfaab23204cfe2bb6014c9d01510438a770929

in  upstream
