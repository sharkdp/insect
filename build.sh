#!/bin/bash

npm run browserify
npm run copy

if [[ $* == *--nexe* ]]
then
    echo "Compiling binaries into ./binaries/"
    # Make a directory to hold the binaries. This should not be checked in to version control.
    mkdir binaries
    # Remove the shebang from index.js.
    tail -n +2 index.js > temp.js
    # We will target windows, mac, and linux.
    # Notice that a nexe build target string takes the form
    # platform-architecture-node_version.
    # If we want to bump the node version we are using,
    # we need to change these lines.
    nexe temp.js -t linux-x64-8.8.1 -o ./binaries/insect-linux-x64
    nexe temp.js -t linux-x86-8.8.1 -o ./binaries/insect-linux-x86
    nexe temp.js -t mac-x64-8.8.1 -o ./binaries/insect-mac
    nexe temp.js -t windows-x64-8.8.1 -o ./binaries/insect-windows-x64.exe
    nexe temp.js -t windows-x86-8.8.1 -o ./binaries/insect-windows-x86.exe
    rm temp.js
    echo "Done compiling binaries."
fi
