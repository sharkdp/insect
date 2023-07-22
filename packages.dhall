{-
Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
  * How to override a package in the package set with a local one: https://github.com/purescript/spago#override-a-package-in-the-package-set-with-a-local-one
  * How to override a package in the package set with a remote one: https://github.com/purescript/spago#override-a-package-in-the-package-set-with-a-remote-one
  * How to add a package to the package set: https://github.com/purescript/spago#add-a-package-to-the-package-set
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230721/packages.dhall
        sha256:8800ac7d0763826544ca3ed3ba61f9dcef761a9e2a1feee0346437d9b861e78f

in  upstream
