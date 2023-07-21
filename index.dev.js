#!/usr/bin/env node

import * as Insect from "./output/Insect/index.js";
import * as path from "path";
import * as xdgBasedir from "xdg-basedir";

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

// Top-level await is not supported in Node 12 and earlier.
(async function() {
  if (process.env.INSECT_NO_RC !== "true") {
    var [util, lineReader, os] = await Promise.all([import("util"), import("line-reader"), import("os")]);

    var rcFilePaths = [path.join(xdgBasedir.config, "insect/insectrc"), path.join(os.homedir(), ".insectrc")];
    var eachLine = util.promisify(lineReader.eachLine);

    for (const rcFilePath of rcFilePaths) {
      try {
        await eachLine(rcFilePath, function(line) {
          var res = runInsect(Insect.fmtPlain, line);
          // We really only care when it breaks
          if (res && res.msgType === "error") {
            console.error(res.msg);
            process.exit(1);
          }
        });
        break;
      } catch (err) {
        if (err.code !== "ENOENT") {
          throw err;
        }
      }
    }
  }

  // Handle command line arguments
  var args = process.argv.slice(2);
  if (args[0] === "-h" || args[0] === "--help") {
    usage();
  } else if (args.length !== 0) {
    // Execute a single command
    var res = runInsect(Insect.fmtPlain, args.join(" "));
    if (res.msgType === "value" || res.msgType === "info") {
      console.log(res.msg);
    } else if (res.msgType === "error") {
      console.error(res.msg);
      process.exit(1);
    }
    process.exit(0);
  }

  var interactive = process.stdin.isTTY;

  if (interactive) {
    var [fs, clipboardy, readline] = await Promise.all([import("fs"), import("clipboardy"), import("readline")]);

    // Create `xdgBasedir.data` if it doesn't already exist.
    // See https://github.com/sharkdp/insect/issues/364.
    try {
      fs.mkdirSync(xdgBasedir.data, { recursive: true });
    } catch (err) {
      if (err.code !== "EEXIST") {
        throw err;
      }
    }

    // Open the history file for reading and appending.
    var historyFd = fs.openSync(path.join(xdgBasedir.data, "insect-history"), 'a+');

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
    }).on('SIGINT', function() {
      rl.clearLine();
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
}());
