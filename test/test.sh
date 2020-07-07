#!/bin/sh

set -eu

BUILD=build
WRITEUP=$BUILD/writeup

rm -rf $BUILD && mkdir $BUILD

export SAVANNA_LIBRARIES=$(pwd)/libraries

sml -m ../src/util/sources.cm ../src/core/sources.cm $/json-lib.cm test.sml $WRITEUP.tex

latexmk \
  -f -pdf \
  -latexoption='-shell-escape' -latexoption='-interaction=nonstopmode' \
  -jobname=$WRITEUP \
  $WRITEUP.tex
