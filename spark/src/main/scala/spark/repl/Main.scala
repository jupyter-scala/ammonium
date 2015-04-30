package spark.repl

object Main0 {
  def interp = {
    Console.err println s"Spark interp called"
    Thread.dumpStack()
    this
  }
}
