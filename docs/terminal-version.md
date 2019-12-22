Terminal version
----------------

In addition to the web interface, there is also a command line version
which can by installed via [npm](https://www.npmjs.com/package/insect):

    npm install -g insect

Note that this might fail if you run it with `sudo`. Instead,
[set up a prefix directory](https://github.com/sindresorhus/guides/blob/master/npm-global-without-sudo.md#install-npm-packages-globally-without-sudo-on-macos-and-linux)
and call `npm install` as a user.

If you prefer not to install nodejs and npm, you can use one of the
standalone binaries on the [release
page](https://github.com/sharkdp/insect/releases).

For Arch Linux, there is a [package on
AUR](https://aur.archlinux.org/packages/insect/):

    yaourt -S insect

On Fedora 28+, you can install `insect` from the official sources:

    sudo dnf install insect

For Fedora versions 26 and 27, you need to enable this [copr
repository](https://copr.fedorainfracloud.org/coprs/fnux/insect/):

    sudo dnf copr enable fnux/insect

For macOS, there is a [Homebrew package](https://formulae.brew.sh/formula/insect):

    brew install insect
