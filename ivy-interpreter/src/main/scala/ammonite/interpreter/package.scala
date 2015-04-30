package ammonite

package object interpreter {

  type IvyPPrintInterpreter = Interpreter[Preprocessor.Output, Iterator[String]]

}
