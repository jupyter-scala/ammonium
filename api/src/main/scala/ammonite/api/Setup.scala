package ammonite.api

trait Setup {
  def apply(modules: String*): Unit
}
