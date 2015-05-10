package ammonite.interpreter

object Wrap {

  def obj(code: String, displayCode: String, previousImportBlock: String, wrapperName: String): String =
    s"""$previousImportBlock

        object $wrapperName$$Main extends AnyRef {
          def $$main() = {val $$user = $wrapperName; $displayCode}
        }

        object $wrapperName{
          $code
        }
     """

  def cls(code: String, displayCode: String, previousImportBlock: String, wrapperName: String): String =
    s"""object $wrapperName extends AnyRef {
          val INSTANCE = new $wrapperName
        }

        object $wrapperName$$Main extends AnyRef {
          $previousImportBlock

          def $$main() = {
            val $$execute = $wrapperName.INSTANCE
            import $wrapperName.INSTANCE.$$user
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
