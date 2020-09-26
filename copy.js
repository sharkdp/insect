const { copy } = require("fs-extra");

async function copyToWeb() {
  await Promise.all([
    copy("insect.js", "web/insect.js"),
    copy("node_modules/keyboardevent-key-polyfill/index.js", "web/keyboardevent-key-polyfill.js"),
    copy("node_modules/jquery/dist/jquery.min.js", "web/jquery.min.js"),
    copy("node_modules/jquery.terminal/js/jquery.terminal.min.js", "web/jquery.terminal.min.js"),
    copy("node_modules/jquery.terminal/js/jquery.mousewheel-min.js", "web/jquery.mousewheel-min.js"),
    copy("node_modules/jquery.terminal/css/jquery.terminal.min.css", "web/terminal.css"),
  ])
}

copyToWeb()
