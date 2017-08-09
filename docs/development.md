Development
-----------
[![Build Status](https://api.travis-ci.org/sharkdp/insect.svg?branch=master)](https://travis-ci.org/sharkdp/insect)

Insect is written in PureScript (see [Getting Started](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md) guide). You can install all dependencies and build the whole project by running:
```
bower install
npm install
pulp -w browserify --skip-entry-point -m Insect --standalone Insect -O -t insect.js
```

Insect comes with a comprehensive set of [unit tests](test/Main.purs). You can run them by calling
```
pulp test
```
