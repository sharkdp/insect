{-
Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
  * How to override a package in the package set with a local one: https://github.com/purescript/spago#override-a-package-in-the-package-set-with-a-local-one
  * How to override a package in the package set with a remote one: https://github.com/purescript/spago#override-a-package-in-the-package-set-with-a-remote-one
  * How to add a package to the package set: https://github.com/purescript/spago#add-a-package-to-the-package-set
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230528/packages.dhall
        sha256:7f1ebc6968ebabae915640456faf98f52f5606189f48cb22305fbe008723f26c

in  upstream
