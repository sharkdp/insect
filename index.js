#!/usr/bin/env node

var Insect = require('./output/Insect/index.js');

var insectEnv = Insect.initialEnvironment;

function usage() {
  console.log("Usage:  insect [EXPR]");
  process.exit(1);
}

function runInsect(line) {
  var lineTrimmed = line.trim();
  if (lineTrimmed === "" || lineTrimmed[0] === "#") {
    return undefined;
  }

  // Run insect
  var res = Insect.repl(insectEnv)(line);

  // Update environment
  insectEnv = res.newEnv;

  return res;
}

// Handle command line arguments
if (process.argv.length >= 4) {
  usage();
} else if (process.argv.length == 3) {
  var arg = process.argv[2];
  if (arg === "-h" || arg === "--help") {
    usage();
  } else {
    var res = runInsect(arg);
    console.log(res.msg);
    process.exit(0);
  }
}

function colored(col, str) {
  return '\x1b[' + col + 'm' + str + '\x1b[0m';
}

var interactive = process.stdin.isTTY;

if (interactive) {
  var readline = require('readline');

  // Set up REPL
  var rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: '> '
  });

  rl.prompt();

  rl.on('line', function(line) {
    var res = runInsect(line);

    if (res) {
      // Format output
      var msg = res.msg;
      if (res.msgType === "value" || res.msgType === "value-set") {
        msg = "\n  " + colored("36", msg);
      } else if (res.msgType == "error") {
        msg = "\n  " + colored("31", msg);
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
} else {
  // Read from non-interactive stream (shell pipe)

  var lineReader = require("line-reader");
  lineReader.eachLine(process.stdin, function(line) {
    var res = runInsect(line);
    if (res) {
      // Only output values and halt on errors. Ignore other message types.
      if (res.msgType === "value") {
        console.log(res.msg);
      } else if (res.msgType == "error") {
        console.error("Error: " + res.msg);
        process.exit(1);
      }
    }
  });
}
