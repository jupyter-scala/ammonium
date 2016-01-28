package ammonite.api

trait Setup {
  def apply(modules: String*): Unit =
    loadAll(modules.map((_, Map.empty[String, String])))
  def load(module: String, versions: Map[String, String] = Map.empty) =
    loadAll(Seq((module, versions)))

  def loadAll(modVer: Seq[(String, Map[String, String])]): Unit
}
