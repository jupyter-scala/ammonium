package ammonite.repl

import ammonite.runtime._
import ammonite.util._

class ReplApiImpl(interp: Interpreter,
                  width0: => Int,
                  height0: => Int,
                  colors0: Ref[Colors],
                  prompt0: Ref[String],
                  frontEnd0: Ref[FrontEnd],
                  history0: => History,
                  sess0: Session,
                  replArgs0: Seq[Bind[_]]) extends RuntimeApiImpl(interp, width0, height0, colors0, history0, sess0, replArgs0) with ReplAPI{

  val prompt = prompt0
  val frontEnd = frontEnd0

}