package ammonite.interpreter

object Wrap {
  def apply(code: String, displayCode: String, previousImportBlock: String, wrapperName: String, classWrap: Boolean): String =
    if (classWrap)
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
    else
      s"""$previousImportBlock

          object $wrapperName$$Main {
            def $$main() = {val $$user = $wrapperName; $displayCode}
          }

          object $wrapperName{
            $code
          }
       """
}
