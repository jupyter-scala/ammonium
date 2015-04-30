package ammonite

import ammonite.compiler.{Preprocessor, Interpreter}

package object interpreter {

  type IvyPPrintInterpreter = Interpreter[Preprocessor.Output, Iterator[String]]

}
