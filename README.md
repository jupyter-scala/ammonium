# ammonium

*Modified Ammonite, for big data frameworks and use from jupyter-scala*

This repository is a slightly modified [Ammonite](https://github.com/lihaoyi/Ammonite), aiming at being fine with big data frameworks (currently specific uses of Spark, POC for Flink), and to be re-used from [jupyter-scala](https://github.com/alexarchambault/jupyter-scala). Its goal is to stay merge-compatible with the original Ammonite, so that it can profit from its latest fixes and features - and in the hope that the changes of this repository can be incorporated into it at some point.

The changes added in this repository fall into the following categories:
- `tweak`: minor changes to the original Ammonite,
- `lib`: aim at making it easier to use Ammonite from other projects (mostly jupyter-scala),
- `fork`: specific to this fork, with no relevance for mainline Ammonite,
- `api`: changes in the user-facing Ammonite API,
- `serialization`: changes to make Ammonite "Java serialization-friendly" (so that Spark, Flink, etc., can serialize things from the REPL).

All commits specific to this repository are tagged with one of these categories.

Support for Spark, POC for Flink, are (will be) in the [jupyter-scala](https://github.com/alexarchambault/jupyter-scala) repository.

## Running it

Ensure [coursier](https://github.com/alexarchambault/coursier) is in your path (`$ coursier --help` should print the version of coursier, among other things). Then download and run the [`ammonium` script](https://github.com/alexarchambault/ammonium/blob/master/ammonium) of this repository.

## Limitations

Test coverage of the added features is rather poor for now.

3 features (4 unit tests) of mainline Ammonite are broken for now:
- support for macros (because the user code is put in classes here, rather than singletons, for serialization-friendliness),
- support for cell private fields (like `private val n = 2; n + 1`, because of putting user code in classes too),
- support for saving / roll-backing sessions.
