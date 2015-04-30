package ammonite.spark
package localcluster

import ammonite.shell.classwrapper.AmmoniteClassWrapperChecker

object SparkTests extends tests.SparkTests(new AmmoniteClassWrapperChecker, master = "local-cluster[1,1,512]")
