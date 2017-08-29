#!/bin/bash

pulp browserify --skip-entry-point -m Insect --standalone Insect -O -t insect.js

cp insect.js web/

cp bower_components/keyboardevent-key-polyfill/index.js web/keyboardevent-key-polyfill.js
cp bower_components/jquery.terminal/js/jquery.terminal-1.6.3.min.js web/jquery.terminal.min.js
cp bower_components/jquery/dist/jquery.min.js web/
cp bower_components/jquery.terminal/js/jquery.mousewheel-min.js web/
