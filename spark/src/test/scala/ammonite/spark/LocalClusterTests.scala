package ammonite.spark

import ammonite.shell.classwrapper.AmmoniteClassWrapperChecker
import ammonite.shell.tests.SparkTests

object LocalClusterTests extends SparkTests(new AmmoniteClassWrapperChecker(sharedLoader = true), "local-cluster[1,1,512]", sparkVersion)
