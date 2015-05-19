package special

package object wrap {

  /**
   * If user code is wrapped in classes by default in the current REPL, importing this wraps the submitted lines
   * in objects rather than classes.
   */
  case object obj

}
