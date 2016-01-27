package ammonite.shell

package object tests {

  val defaultWrapper: (Int, Int) => String =
    if (is210)
      (_, _) => "$user."
    else
      (_, _) => ""

}
