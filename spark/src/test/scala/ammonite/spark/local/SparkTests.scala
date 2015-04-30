package ammonite.spark
package local

object SparkTests extends tests.SparkTests(master = "local", broadcastOk = false)
