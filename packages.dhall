{-
Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
  * How to override a package in the package set with a remote one: https://github.com/purescript/spago#override-a-package-in-the-package-set-with-a-remote-one
  * How to add a package to the package set: https://github.com/purescript/spago#add-a-package-to-the-package-set
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220725/packages.dhall
        sha256:e56fbdf33a5afd2a610c81f8b940b413a638931edb41532164e641bb2a9ec29c

in  upstream
