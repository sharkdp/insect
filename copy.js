const fs = require("fs-extra")

fs.copySync(
  "insect.js",
  "web/insect.js",
)
fs.copySync(
  "bower_components/keyboardevent-key-polyfill/index.js",
  "web/keyboardevent-key-polyfill.js",
)
fs.copySync(
  "bower_components/jquery/dist/jquery.min.js",
  "web/jquery.min.js",
)
fs.copySync(
  "bower_components/jquery.terminal/js/jquery.terminal-1.6.3.min.js",
  "web/jquery.terminal.min.js",
)
fs.copySync(
  "bower_components/jquery.terminal/js/jquery.mousewheel-min.js",
  "web/jquery.mousewheel-min.js",
)
