package ammonite.spark
package standalonecluster

import ammonite.shell.classwrapper.AmmoniteClassWrapperChecker

object SparkTests extends tests.SparkTests(new AmmoniteClassWrapperChecker, master = "spark://master:7077")
