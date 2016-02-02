## ammonium

A modified version of [Ammonite](https://github.com/lihaoyi/Ammonite/).

[![Build Status](https://travis-ci.org/alexarchambault/ammonite-shell.svg)](https://travis-ci.org/alexarchambault/ammonite-shell)

If you're not using Spark, or are not interested in advanced features like
class loader isolation, you should really be using [Ammonite](https://github.com/lihaoyi/Ammonite/). If you prefer web-based notebook interfaces to terminal
ones, you should be using [jupyter-scala](https://github.com/alexarchambault/jupyter-scala). Else, welcome here, this is ammonium, a - possibly temporary, and somehow experimental - fork of Ammonite, and the basis of [jupyter-scala](https://github.com/alexarchambault/jupyter-scala)!

Compared to the original Ammonite, it adds, among others, the following features:

* Java serialization friendly (Spark support through an external dependency loaded from the REPL),
* cleaner user-facing ClassLoader (less dependencies -> cleaner completion, easier to move the session JARs around),
* better dependencies management (faster module resolution with [coursier](https://github.com/alexarchambault/coursier)),
* easier to re-use in other contexts (in particular, [jupyter-scala](https://github.com/alexarchambault/jupyter-scala), the initial motivation of this repo).

Most of these require some hard-to-justify-a-priori overhauling of the code, hence this fork.


## Try it

Download and run its launcher with
```
curl -L -o ammonium https://git.io/vzNa2 && chmod +x ammonium && ./ammonium
```

For the 2.10 version, do
```
curl -L -o ammonium-2.10 https://git.io/vzApn && chmod +x ammonium-2.10 && ./ammonium-2.10
```

## Usage

### Adding dependencies

Example

```scala
@ classpath.add("org.typelevel" %% "cats" % "0.4.0")
```

Several dependencies can be added at the same time:
```scala
@ classpath.add(
    "org.typelevel" %% "cats" % "0.4.0",
    "com.chuusai" %% "shapeless" % "2.2.5"
  )
```

### Adding compiler plugin

Compiler plugins are like standard dependencies, except that they are added to a separate
configuration - here `"plugin"` instead of the default `"compile"`.

```scala
@ classpath.addInConfig("plugin")(
    "org.spire-math" %% "kind-projector" % "0.7.1"
  )
Adding 1 artifact(s)

@ trait TC[F[_]]
defined trait TC

@ type TCEitherStr = TC[Either[String, ?]]
defined type TCEitherStr
```

### Easier dependency adding with setups

Setups allow to easily add a bunch of dependencies in the session, and run
a few lines that typically initialize what was added.

Example:

```
@ setup("shapeless") // load shapeless and add its main imports
...

@ (1, "a") :+ true
res1: (Int, String, Boolean) = (1, "a", true)

@ 1.narrow
BAM - not supported by Ammonite's pprint
```

```
@ setup("cats")
...


```

### Other

Manually evaluate code:
```scala
@ eval("print(2)")
...
```

List what's in the classpath:
```scala
@ show(classpath.path()) // inspects the default configuration, compile
...

@ show(classpath.path("plugin")) // look at the compiler plugins configuration
...
```

Print a value, with no truncation (same as in Ammonite):
```scala
@ show(List.fill(40)("a"))
...
```

## Internals

ammonium's fork started - then diverged - from the original Ammonite sources around mid-2015.

The first two notable changes consist in splitting the `repl` module of Ammonite in 2, then
each of the resulting modules in 2 again, resulting in 4 modules. The first split separates
the user-facing API (`interpreter-api` and `shell-api` modules in ammonium) from the internals
of the interpreter (`interpreter` and `shell` in ammonium). Each of them are split in 2 again,
separating the core of the interpreter (`interpreter-api` and `interpreter`) from the
terminal-specific parts (`shell-api` and `shell`). So the 4 resulting modules are:

* `interpreter`: the core of the interpreter, able to compile code and dealing with byte code and classpaths,
* `interpreter-api`: the user-facing parts of the core of the interpreter - contains definitions of interfaces that will be seen by the users, like `Classpath`, `Eval`, or `Setup`,
* `shell`: a terminal-based interface for the interpreter, with roughly the same features as `repl` from Ammonite,
* `shell-api`: the `shell`-specific user-facing parts of `shell` - defines `Term` in particular.

`interpreter` and `shell-api` depend on `interpreter-api`. `shell` depends on the 3 others.

## Launcher internals

The launchers heavily use the possibilities offered by [coursier](https://github.com/alexarchambault/coursier).
With the coursier launcher, one could trivially launch ammonium with
```
$ ./coursier launch com.github.alexarchambault.ammonium:shell_2.11.7:0.4.0-M2
```

Launching ammonium this way prevents class loader isolation to work though: the ammonium dependencies are
mixed up with the user ones, in the REPL session.

For class loader isolation to work, one must ask the coursier launcher to setup separate class loaders
with just the right dependencies, alongside the class loader that loads and runs ammonium. This can be
achieved with
```
$ ./coursier launch \
    com.github.alexarchambault.ammonium:shell_2.11.7:0.4.0-M2 \
    -i ammonium-compile,ammonium-macro \
    -I ammonium-compile:com.github.alexarchambault.ammonium:shell-api_2.11.7:0.4.0-M2 \
    -I ammonium-macro:org.scala-lang:scala-compiler:2.11.7
```

The `-i ammonium-compile,ammonium-macro` options asks the coursier launcher to setup two
separate class loaders (alongside the main one), `ammonium-compile` and `ammonium-macro`.
The class loaders setup by the launcher form a hierarchy, that is `ammonium-compile` will
be the parent of `ammonium-macro`, which will itself be the parent of the class loader
that will load the remaining dependencies.

The `-I ammonium-compile:com.github.alexarchambault.ammonium:shell-api_2.11.7:0.4.0-M2` option
asks the launcher to put the dependency `com.github.alexarchambault.ammonium:shell-api_2.11.7:0.4.0-M2`,
along with its transitive dependencies, in the `ammonium-compile` loader. In the same
fashion, `-I ammonium-macro:org.scala-lang:scala-compiler:2.11.7` asks the launcher to put
`org.scala-lang:scala-compiler:2.11.7` and its transitive dependencies in the `ammonium-macro` loader.
The remaining dependencies of `com.github.alexarchambault.ammonium:shell_2.11.7:0.4.0-M2` will
be put in a third class loader, inheriting `ammonium-macro`.

Launched this way, ammonium will find the `ammonium-compile` and `ammonium-macro` loaders,
and use them to setup class loader isolation in the session. `ammonium-compile` will be
the basis of the class loader of the session, and `ammonium-macro` will be the basis of the
loader that runs macros.

The launchers that are linked above are generated by
[a separate script](https://github.com/alexarchambault/ammonium/blob/master/project/generate-launcher.sh),
that asks coursier to generate a bootstrap launcher, which downloads all the dependencies of
ammonium in a first time, then launch ammonium with the same class loader setup as above.

## Notice

Copyright 2015-2016, Alexandre Archambault

Original Ammonite code is Copyright 2014-2015, Li Haoyi

Released under a MIT license
