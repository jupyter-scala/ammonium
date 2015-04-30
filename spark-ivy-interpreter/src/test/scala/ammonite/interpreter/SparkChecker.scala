package ammonite.interpreter

class SparkChecker extends Checker {
  override def newInterpreter(): Interpreter[Preprocessor.Output, Iterator[String]] =
    ???
}
