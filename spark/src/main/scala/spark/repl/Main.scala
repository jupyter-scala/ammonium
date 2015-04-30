package spark.repl

object Main {
  def interp = {
    Console.err println s"Spark interp called"
    Thread.dumpStack()
    this
  }
}
