package ammonite.repl

import ammonite.runtime.APIHolder
import ammonite.util.Ref

trait ReplAPI extends RuntimeAPI {
  /**
    * Read/writable prompt for the shell. Use this to change the
    * REPL prompt at any time!
    */
  val prompt: Ref[String]
  /**
    * The front-end REPL used to take user input. Modifiable!
    */
  val frontEnd: Ref[FrontEnd]

}

object ReplBridge extends APIHolder[FullRuntimeAPI with ReplAPI]
