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

  def classWrapImportsTransform(r: Res[Evaluated[_]]): Res[Evaluated[_]] =
    r .map { ev =>
      ev.copy(imports = ev.imports.map{ d =>
        if (d.prefix.startsWith(d.wrapperName + ".$ref$")) {
          // Assuming this is an import through previous REPL variables
          val stripped = d.prefix.stripPrefix(d.wrapperName + ".$ref$")
          d.copy(prefix = stripped, wrapperName = stripped.takeWhile(_ != '.'))
        } else
          d
      })
    }

}
