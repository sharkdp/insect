{-
Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
  * How to override a package in the package set with a remote one: https://github.com/purescript/spago#override-a-package-in-the-package-set-with-a-remote-one
  * How to add a package to the package set: https://github.com/purescript/spago#add-a-package-to-the-package-set
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220531/packages.dhall
        sha256:278d3608439187e51136251ebf12fabda62d41ceb4bec9769312a08b56f853e3

in  upstream
