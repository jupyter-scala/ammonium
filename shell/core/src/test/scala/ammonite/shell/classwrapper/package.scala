package ammonite.shell

package object classwrapper {

  val wrapper: (Int, Int) => String =
    if (is210)
      (_, _) => "INSTANCE.$ref$"
    else
      (_, _) => ""

}
