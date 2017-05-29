package ammonite

object TestMain{
  def main(args: Array[String]): Unit = {
    val homeFlag = Array("--home", "target/tempAmmoniteHome")
    args match{
      case Array(first, rest@_*) if first.startsWith("--") => Main.main(args ++ homeFlag)
      case Array(first, rest@_*) => Main.main(Array(first) ++ homeFlag ++ rest)
      case _ => Main.main(homeFlag ++ args)
    }
  }
}