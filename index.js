#!/usr/bin/env node

import * as Insect from "./insect.js";
import * as path from "path";

var insectEnv = Insect.initialEnvironment;

function usage() {
  console.log("Usage:  insect [EXPR]");
  process.exit(1);
}

function runInsect(fmt, line) {
  var lineTrimmed = line.trim();
  if (lineTrimmed === "" || lineTrimmed[0] === "#") {
    return undefined;
  }

  // Run insect
  var res = Insect.repl(fmt)(insectEnv)(line);

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
    // Execute a single command
    var res = runInsect(Insect.fmtPlain, arg);
    if (res.msgType === "value" || res.msgType === "info") {
      console.log(res.msg);
    } else if (res.msgType === "error") {
      console.error(res.msg);
    }
    process.exit(0);
  }
}

if (process.env.INSECT_NO_RC !== "true") {
  // Node 12 does not support top-level await.
  (async function() {
    var [os, lineReader] = await Promise.all([import("os"), import("line-reader")]);

    var rcFile = path.join(os.homedir(), ".insectrc");
    lineReader.eachLine(rcFile, function (line) {
      var res = runInsect(Insect.fmtPlain, line);
      // We really only care when it breaks
      if (res && res.msgType === "error") {
        console.error(res.msg);
        process.exit(1);
      }
    }, function (err) {
      // If the file doesn't exist, that's fine
      if (err && err.code !== "ENOENT") {
        throw err;
      } else {
        startInsect();
      }
    });
  })();
} else {
  startInsect();
}

async function startInsect() {
  var interactive = process.stdin.isTTY;

  if (interactive) {
    var [fs, clipboardy, readline, xdgBasedir] = await Promise.all([import("fs"), import("clipboardy"), import("readline"), import("xdg-basedir")]);

    // Open the history file for reading and appending.
    var historyFd = fs.openSync(path.join(xdgBasedir.xdgData, "insect-history"), 'a+');

    var maxHistoryLength = 5000;

    // Set up REPL
    var rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      history: fs.readFileSync(historyFd, "utf8").split("\n").slice(0, -1).reverse().slice(0, maxHistoryLength),
      historySize: maxHistoryLength,
      completer: function(line) {
        var identifiers = Insect.identifiers(insectEnv);

        var keywords =
          identifiers.concat(Insect.functions(insectEnv), Insect.supportedUnits, Insect.commands);

        var lastWord = line;
        if (line.trim() !== "") {
          var words = line.split(/\b/);
          lastWord = words[words.length - 1];
          keywords = keywords.filter(function(kw) {
            return kw.indexOf(lastWord) === 0;
          });
        }

        return [keywords, lastWord];
      },
    });

    var prompt = '\x1b[01m>>>\x1b[0m ';

    // The visual length of the prompt (4) needs to be set explicitly for
    // older versions of node:
    rl.setPrompt(prompt, 4);

    rl.on('line', function(line) {
      var res = runInsect(Insect.fmtConsole, line);

      if (res) {
        if (res.msgType === "quit") {
          process.exit(0);
        } else if (res.msgType === "clear") {
          process.stdout.write('\x1Bc');
        } else if (res.msgType === "copy") {
          if (res.msg === "") {
            console.log("\nNo result to copy.\n");
          } else {
            clipboardy.writeSync(res.msg);
            console.log("\nCopied result '" + res.msg + "' to clipboard.\n");
          }
        } else {
          console.log(res.msg + "\n");
        }
      }

      rl.prompt();
    }).on('close', function() {
      process.exit(0);
    });

    // `createWriteStream` doesn't care about the first argument if it's given
    // `fd`.
    var historyStream = fs.createWriteStream(undefined, {fd: historyFd});

    var oldAddHistory = rl._addHistory;

    // TODO: figure out how to do this without resorting to Node.js internals.
    rl._addHistory = function() {
      var last = rl.history[0];

      var line = oldAddHistory.call(rl);

      if (line && line !== last) {
        historyStream.write(line + "\n");
      }

      return line;
    };

    rl.prompt();
  } else {
    if (typeof lineReader === "undefined") {
      var lineReader = await import("line-reader");
    }

    // Read from non-interactive stream (shell pipe)
    lineReader.eachLine(process.stdin, function(line) {
      var res = runInsect(Insect.fmtPlain, line);
      if (res) {
        // Only output values and halt on errors. Ignore 'info' and 'value-set'
        // message types.
        if (res.msgType === "value") {
          console.log(res.msg);
        } else if (res.msgType == "error") {
          console.error(res.msg);
          process.exit(1);
        } else if (res.msgType == "quit") {
          process.exit(0);
        }
      }
    });
  }
}
