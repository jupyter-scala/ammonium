package ammonite.spark

import ammonite.shell.classwrapper.AmmoniteClassWrapperChecker
import ammonite.shell.tests.SparkTests

object StandAloneClusterTests extends SparkTests(new AmmoniteClassWrapperChecker(sharedLoader = true), "spark://master:7077", sparkVersion)
