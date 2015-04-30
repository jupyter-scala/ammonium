package ammonite.interpreter

object Wrap {

  def obj(code: String, displayCode: String, previousImportBlock: String, wrapperName: String): String =
    s"""$previousImportBlock

        object $wrapperName{
          $code
          def $$main() = {$displayCode}
        }
     """

  def cls(code: String, displayCode: String, previousImportBlock: String, wrapperName: String, instanceSymbol: String): String =
    s"""object $wrapperName extends AnyRef {
          val $instanceSymbol = new $wrapperName
        }

        object $wrapperName$$Main extends AnyRef {
          $previousImportBlock

          def $$main() = {
            val $$execute = $wrapperName.$instanceSymbol
            import $wrapperName.$instanceSymbol.$$user
            $displayCode
          }
        }


        class $wrapperName extends Serializable {
          $previousImportBlock

          class $$user extends Serializable {
            $code
          }

          val $$user = new $$user
        }
     """

}
