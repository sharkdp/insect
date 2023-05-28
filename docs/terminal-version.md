Terminal version
----------------

In addition to the web interface, there is also a command-line version
(supporting Node.js 10 and later) which can by installed via
[npm](https://www.npmjs.com/package/insect):

    npm install -g insect

Note that you should almost always never run this as root or with `sudo`. If the
command fails due to permission issues, [set up a prefix
directory](https://github.com/sindresorhus/guides/blob/master/npm-global-without-sudo.md#install-npm-packages-globally-without-sudo-on-macos-and-linux)
and call `npm install` as a user instead.

For Arch Linux, there is an [AUR
package](https://aur.archlinux.org/packages/insect/):

    yaourt -S insect

For macOS, there is a [Homebrew formula](https://formulae.brew.sh/formula/insect):

    brew install insect

For Android, install [Termux](https://termux.com/) from [F-Droid](https://f-droid.org/packages/com.termux/). Install Node.js in Termux and then install `insect` from npm:

    pkg install nodejs-lts
    npm install -g insect
