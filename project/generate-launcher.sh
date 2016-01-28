#!/bin/bash

VERSION=0.4.0-SNAPSHOT

"$(dirname "$0")/../coursier" bootstrap \
  com.github.alexarchambault.ammonium:shell_2.11.7:$VERSION \
  -I ammonium-compile:com.github.alexarchambault.ammonium:shell-api_2.11.7:$VERSION \
  -I ammonium-macro:org.scala-lang:scala-compiler:2.11.7 \
  -i ammonium-compile,ammonium-macro \
  -r central \
  -D "\${user.home}/.ammonium/bootstrap/$VERSION" \
  -f -o ammonium \
  -M ammonite.Ammonite
