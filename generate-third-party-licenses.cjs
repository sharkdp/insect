#!/usr/bin/env node

// Generates `LICENSE_THIRDPARTY` (for the Insect CLI) and
// `web/third-party-licenses.txt` (for the Insect website) files that include
// the licenses of third-party software used by Insect.

const exec = require("util").promisify(require("child_process").exec);
const fs = require("fs");
const { join: pathJoin } = require("path");

function processDeps(deps) {
  let thirdPartyLicenses = "";

  for (const dep of deps) {
    thirdPartyLicenses += `-------------------------------------------------------------------------------------\n${dep.name} ${dep.version}\n\n`;

    if (dep.name === "@jcubic/lily") {
      // This package has no license file, so use notice from
      // https://github.com/jcubic/lily/blob/6f40c5b01af540248e546be183f97338be864097/index.js#L4-L7
      // for now.
      //
      // Also see https://github.com/jcubic/lily/issues/8.
      thirdPartyLicenses +=
        "Copyright (C) 2020-2021 Jakub T. Jankiewicz\n\nReleased under MIT license\n\n";
      continue;
    }

    const licenseFiles = fs
      .readdirSync(dep.path)
      .filter((file) => /^(licen[sc]e|copying|notice)/i.test(file))
      .map((file) => pathJoin(dep.path, file));

    if (licenseFiles.length === 0) {
      console.error(
        `Could not find any license files for ${dep.name} ${dep.version}`
      );
      process.exit(1);
    }

    for (const licenseFile of licenseFiles) {
      thirdPartyLicenses += fs
        .readFileSync(licenseFile, { encoding: "utf8" })
        .replaceAll("\r\n", "\n") // Some license files use DOS newlines.
        .trim();
      thirdPartyLicenses += "\n\n";
    }
  }

  return thirdPartyLicenses.trim();
}

(async () => {
  const [npmDeps, spagoDeps] = await Promise.all([
    exec("npm ls --json --long --omit dev --all").then(({ stdout }) => {
      function go(json) {
        const deps = [];

        for (const depName in json) {
          const dep = json[depName];

          // `fsevents` has an undefined path and version for whatever reason.
          if (dep.path === undefined) {
            continue;
          }

          deps.push({ name: depName, path: dep.path, version: dep.version });

          if (dep.dependencies) {
            deps.push(...go(dep.dependencies));
          }
        }

        return deps;
      }

      return go(JSON.parse(stdout).dependencies);
    }),
    exec("npx spago ls deps --transitive --json").then(({ stdout }) =>
      stdout
        .trim()
        .split("\n")
        .map((line) => {
          const { packageName, version } = JSON.parse(line);
          return {
            name: `purescript-${packageName}`,
            // Remove `v` version prefix, if any.
            version: version.replace(/^v/, ""),
            path: pathJoin(".spago", packageName, version),
          };
        })
    ),
  ]);

  const npmDepsLicenses = processDeps(npmDeps);
  const spagoDepsLicenses = processDeps(spagoDeps);

  fs.writeFileSync("LICENSE_THIRDPARTY", spagoDepsLicenses);
  fs.writeFileSync(
    "web/third-party-licenses.txt",
    `${npmDepsLicenses}\n\n${spagoDepsLicenses}`
  );
})();
