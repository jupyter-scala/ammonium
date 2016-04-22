#!/bin/bash

VERSION=0.4.0-M5

if [ "$1" = "--2.10" ]; then
  SCALA_VERSION=2.10.6
  OUTPUT=ammonium-2.10
  EXTRA="
    -E com.chuusai:shapeless_2.10.4 \
    -I ammonium-compile:org.scala-lang:scala-compiler:$SCALA_VERSION \
    -I ammonium-compile:org.scalamacros:quasiquotes_2.10:2.0.1 \
  "
else
  SCALA_VERSION=2.11.8
  OUTPUT=ammonium
  EXTRA=
fi

"$(dirname "$0")/../coursier" bootstrap \
  com.github.alexarchambault.ammonium:shell_$SCALA_VERSION:$VERSION \
  -I ammonium-compile:com.github.alexarchambault.ammonium:shell-api_$SCALA_VERSION:$VERSION \
  -I ammonium-macro:org.scala-lang:scala-compiler:$SCALA_VERSION \
  $EXTRA \
  -i ammonium-compile,ammonium-macro \
  --no-default \
  -r central \
  -r sonatype:releases \
  -d "\${user.home}/.ammonium/bootstrap" \
  -f -o "$OUTPUT" \
  -M ammonite.Ammonite
