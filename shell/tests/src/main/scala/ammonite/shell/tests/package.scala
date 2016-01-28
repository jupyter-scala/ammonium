package ammonite.shell

package object tests {

  val defaultWrapper =
    if (is210)
      "$user."
    else
      ""

}
