package ammonite.spark
package localcluster

object SparkTests extends tests.SparkTests(master = "local-cluster[1,1,512]")
