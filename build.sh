#!/bin/sh
rm -r elm-stuff/build-artifacts 2> /dev/null
elm make --warn src/Main.elm
patch -p0 -N -s -r - < upload-progress.patch
