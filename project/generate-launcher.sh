#!/bin/bash

VERSION=0.4.0-SNAPSHOT

"$(dirname "$0")/../coursier" bootstrap \
  com.github.alexarchambault:ammonite-shell_2.11.7:$VERSION \
  -r central \
  -D "\${user.home}/.ammonium/bootstrap/$VERSION" \
  -f -o ammonium \
  -M ammonite.Ammonite
