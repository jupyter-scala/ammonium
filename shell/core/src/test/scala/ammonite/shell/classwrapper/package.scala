package ammonite.shell

package object classwrapper {

  val wrapper =
    if (is210)
      "INSTANCE.$ref$"
    else
      ""

}
