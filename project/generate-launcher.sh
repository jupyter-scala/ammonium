#!/bin/bash

VERSION=0.4.0-M2

if [ "$1" = "--2.10" ]; then
  SCALA_VERSION=2.10.6
  OUTPUT=ammonium-2.10
else
  SCALA_VERSION=2.11.7
  OUTPUT=ammonium
fi

"$(dirname "$0")/../coursier" bootstrap \
  com.github.alexarchambault.ammonium:shell_$SCALA_VERSION:$VERSION \
  -I ammonium-compile:com.github.alexarchambault.ammonium:shell-api_$SCALA_VERSION:$VERSION \
  -I ammonium-macro:org.scala-lang:scala-compiler:$SCALA_VERSION \
  -i ammonium-compile,ammonium-macro \
  --no-default \
  -r central \
  -r sonatype:releases \
  -D "\${user.home}/.ammonium/bootstrap" \
  -f -o "$OUTPUT" \
  -M ammonite.Ammonite
