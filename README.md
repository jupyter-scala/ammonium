## ammonite-shell

*An impatient fork of Ammonite*

[![Build Status](https://travis-ci.org/alexarchambault/ammonite-shell.svg)](https://travis-ci.org/alexarchambault/ammonite-shell)

This repo is a fork of the REPL part of [Ammonite](https://github.com/lihaoyi/Ammonite/).
Compared to it, it adds, among others, the following features:

* Java serialization friendly (Spark support through an external dependency loaded from the REPL),
* cleaner user-facing ClassLoader (less dependencies -> cleaner completion, easier to move the session JARs around),
* better dependencies management (adding Ivy/Maven resolvers, module resolution closer to how SBT does it, loading an external SBT project as a dependency, *not wiping out the session variables upon dependency adding* - with Ammonite, these are lazily recomputed),
* easier to use programatically in other contexts (in particular, [jupyter-scala](https://github.com/alexarchambault/jupyter-scala)).

**Detailed list of additionnal features and examples to come**

## Notice

Copyright 2015, Alexandre Archambault

Original Ammonite code is Copyright 2014-2015, Li Haoyi

Released under a MIT license
