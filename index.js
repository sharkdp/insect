#!/usr/bin/env node

var readline = require('readline');
var Insect = require('./output/Insect/index.js');

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  prompt: '> '
});

rl.prompt();

var globalEnv = Insect.initialEnvironment;

function colored(col, str) {
  return '\x1b[' + col + 'm' + str + '\x1b[0m';
}

rl.on('line', function(line) {
  var lineTrimmed = line.trim();
  if (lineTrimmed !== "" && lineTrimmed[0] !== "#") {
    // Run insect
    var res = Insect.repl(globalEnv)(line);
    globalEnv = res.newEnv;

    // Format output
    var msg = res.msg;
    if (res.msgType === "value") {
      msg = "\n  " + colored("36", msg);
    } else if (res.msgType == "error") {
      msg = colored("31", msg);
    } else if (res.msgType == "info") {
      msg = colored("33", msg);
    } else if (res.msgType == "other") {
      msg = msg.replace(/`([^`\n]+)`/g, '\x1b[36m$1\x1b[0m');
      msg = msg.replace(/\*([^\*\n]+)\*/g, '\x1b[01m$1\x1b[0m');
    }

    console.log(msg + "\n");
  }

  rl.prompt();
}).on('close', function() {
  process.exit(0);
});
