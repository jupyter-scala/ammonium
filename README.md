## ammonite-shell

*A modified version of Ammonite*

[![Build Status](https://travis-ci.org/alexarchambault/ammonite-shell.svg)](https://travis-ci.org/alexarchambault/ammonite-shell)

This repo is a modified version of [Ammonite](https://github.com/lihaoyi/Ammonite/).
Compared to it, it adds, among others, the following features:

* Java serialization friendly (Spark support through an external dependency loaded from the REPL),
* cleaner user-facing ClassLoader (less dependencies -> cleaner completion, easier to move the session JARs around),
* better dependencies management (adding Maven repositories, faster module resolution with [coursier](https://github.com/alexarchambault/coursier), *not wiping out the session variables upon dependency adding* - with Ammonite, these are lazily recomputed),
* easier to use programatically in other contexts (in particular, [jupyter-scala](https://github.com/alexarchambault/jupyter-scala), the initial motivation of this repo).

It does not aim at replacing the official Ammonite project by any mean. The motivation for this separate
repository is mainly that its changes don't have to go through the time consuming
[contribution review process](https://github.com/lihaoyi/Ammonite#contribution-guidelines) of Ammonite. Most of these changes
imply overhauling the whole code, and the idea of having to justify every single line change sounds nightmarish to
me!

## Try it

```
$ git clone https://github.com/alexarchambault/ammonite-shell.git
$ cd ammonite-shell
$ sbt shell/pack publishLocal
$ shell/target/pack/bin/ammonite
Loading Ammonite Shell...
@ load.module("com.github.alexarchambault" % "ammonite-spark_1.5_2.11.7" % "0.4.0-SNAPSHOT")
@ @transient val Spark = new ammonite.spark.Spark
log4j:WARN No appenders could be found for logger (org.eclipse.jetty.util.log).
log4j:WARN Please initialize the log4j system properly.
log4j:WARN See http://logging.apache.org/log4j/1.2/faq.html#noconfig for more info.
Spark: Spark = Spark(uninitialized)
@ Spark.withConf(_.setMaster("local[1]"))
@ import Spark.sc
import Spark.sc
@ sc
...
res9: org.apache.spark.SparkContext = SparkContext
@ val accum = sc.accumulator(0)
accum: org.apache.spark.Accumulator[Int] = 0
@ sc.parallelize(1 to 10).foreach(x => accum += x)
...
@ accum.value
res12: Int = 55
```

## Notice

Copyright 2015, Alexandre Archambault

Original Ammonite code is Copyright 2014-2015, Li Haoyi

Released under a MIT license
