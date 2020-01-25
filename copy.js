const fs = require("fs-extra")

fs.copySync(
  "insect.js",
  "web/insect.js",
)
fs.copySync(
  "node_modules/keyboardevent-key-polyfill/index.js",
  "web/keyboardevent-key-polyfill.js",
)
fs.copySync(
  "node_modules/jquery/dist/jquery.min.js",
  "web/jquery.min.js",
)
fs.copySync(
  "node_modules/jquery.terminal/js/jquery.terminal.min.js",
  "web/jquery.terminal.min.js",
)
fs.copySync(
  "node_modules/jquery.terminal/js/jquery.mousewheel-min.js",
  "web/jquery.mousewheel-min.js",
)
fs.copySync(
  "node_modules/jquery.terminal/css/jquery.terminal.min.css",
  "web/terminal.css",
)
