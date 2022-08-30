Terminal version
----------------

In addition to the web interface, there is also a command line version
which can by installed via [npm](https://www.npmjs.com/package/insect):

    npm install -g insect

Note that this might fail if you run it with `sudo`. Instead,
[set up a prefix directory](https://github.com/sindresorhus/guides/blob/master/npm-global-without-sudo.md#install-npm-packages-globally-without-sudo-on-macos-and-linux)
and call `npm install` as a user.

For Arch Linux, there is a [package on
AUR](https://aur.archlinux.org/packages/insect/):

    yaourt -S insect

For macOS, there is a [Homebrew package](https://formulae.brew.sh/formula/insect):

    brew install insect

For Android, install [Termux](https://termux.com/) from [F-Droid](https://f-droid.org/packages/com.termux/). Install Node.js in Termux and then install `insect`.

    pkg install nodejs-lts
    npm install -g insect
