package ammonite.repl


import java.io.File
import java.nio.file.Files

import ammonite.runtime.tools.Resolver
import ammonite.util.{Bind, CodeColors, Colors, Ref}
import ammonite.util.Util.newLine

import scala.collection.mutable
import scala.reflect.runtime.universe._
import acyclic.file
import ammonite.runtime.{APIHolder, Frame, History, ReplExit}

import scala.util.control.ControlThrowable
import acyclic.file



trait RuntimeAPI {
  /**
   * Exit the Ammonite REPL. You can also use Ctrl-D to exit
   */
  def exit = throw ReplExit(())
  /**
   * Exit the Ammonite REPL. You can also use Ctrl-D to exit
   */
  def exit(value: Any) = throw ReplExit(value)
  /**
    * Functions that will be chained and called on the
    * exitValue before the repl exits
    */
  val beforeExitHooks: mutable.Buffer[Any ⇒ Any]

  def onExit(cb: Any => Unit): Unit


  /**
   * Display help text if you don't know how to use the REPL
   */
  def help: String

  /**
    * The last exception that was thrown in the REPL; `null` if nothing has
    * yet been thrown. Useful if you want additional information from the
    * thrown exception than the printed stack trace (e.g. many exceptions have
    * additional metadata attached) or if you want to show the stack trace
    * on an exception that doesn't normally print it (e.g. seeing the stack
    * when a Ctrl-C interrupt happened) via `lastException.printStackTrace`.
    */
  def lastException: Throwable
  /**
   * History of commands that have been entered into the shell, including
   * previous sessions
   */
  def fullHistory: History

  /**
   * History of commands that have been entered into the shell during the
   * current session
   */
  def history: History

  /**
   * Get the `Type` object of [[T]]. Useful for finding
   * what its methods are and what you can do with it
   */
  def typeOf[T: WeakTypeTag]: Type

  /**
   * Get the `Type` object representing the type of `t`. Useful
   * for finding what its methods are and what you can do with it
   *
   */
  def typeOf[T: WeakTypeTag](t: => T): Type

  /**
   * The colors that will be used to render the Ammonite REPL in the terminal
   */
  val colors: Ref[Colors]

  /**
   * Throw away the current scala.tools.nsc.Global and get a new one
   */
  def newCompiler(): Unit

  /**
   * Access the compiler to do crazy things if you really want to!
   */
  def compiler: scala.tools.nsc.Global

  /**
   * Show all the imports that are used to execute commands going forward
   */
  def imports: String

  implicit def tprintColorsImplicit: pprint.TPrintColors


  implicit def codeColorsImplicit: CodeColors

  def pprinter: Ref[pprint.PPrinter]

  implicit def pprinterImplicit = pprinter()

  /**
   * Current width of the terminal
   */
  def width: Int
  /**
   * Current height of the terminal
   */
  def height: Int

  /**
   * Lets you configure the pretty-printing of a value. By default, it simply
   * disables truncation and prints the entire thing, but you can set other
   * parameters as well if you want.
   */

  def show(t: Any,
           width: Integer = 0,
           height: Integer = null,
           indent: Integer = null): Unit
  /**
    * Functions that can be used to manipulate the current REPL session:
    * check-pointing progress, reverting to earlier checkpoints, or deleting
    * checkpoints by name.
    *
    * Frames get pushed on a stack; by default, a saved frame is
    * accessible simply by calling `load`. If you provide a name
    * when `save`ing a checkpoint, it can later be `load`ed directly
    * by providing the same name to `load`
    *
    * Un-named checkpoints are garbage collected, together with their
    * classloader and associated data, when they are no longer accessible
    * due to `restore`. Named checkpoints are kept forever; call `delete`
    * on them if you really want them to go away.
    */
  def sess: Session
}
trait Session{
  /**
    * The current stack of frames
    */
  def frames: List[Frame]
  /**
    * Checkpoints your current work, placing all future work into its own
    * frames. If a name is provided, it can be used to quickly recover
    * that checkpoint later.
    */
  def save(name: String = ""): Unit

  /**
    * Discards the last frames, effectively reverting your session to
    * the last `save`-ed checkpoint. If a name is provided, it instead reverts
    * your session to the checkpoint with that name.
    */
  def load(name: String = ""): SessionChanged

  /**
    * Resets you to the last save point. If you pass in `num`, it resets
    * you to that many savepoints since the last one.
    */
  def pop(num: Int = 1): SessionChanged
  /**
    * Deletes a named checkpoint, allowing it to be garbage collected if it
    * is no longer accessible.
    */
  def delete(name: String): Unit


  /**
    * Generates a JAR containing all the build products of this session.
    */
  def sessionJar(frames: List[Frame] = frames): Array[Byte] = {

    val buffer = new java.io.ByteArrayOutputStream
    val outputZip = new java.util.zip.ZipOutputStream(buffer)

    def putEntry(name: String, content: Array[Byte]): Unit = {
      val entry = new java.util.zip.ZipEntry(name)

      outputZip.putNextEntry(entry)
      outputZip.write(content)
      outputZip.closeEntry()
    }

    for ((name, bytes) <- frames.reverse.flatMap(_.classloader.newFileDict).toMap)
      putEntry(name.replace('.', '/') + ".class", bytes)

    outputZip.close()

    buffer.toByteArray
  }

  def sessionJarFile(output: File, frames: List[Frame]): java.io.File = {
    val b = sessionJar(frames)
    Files.write(output.toPath, b)
    output
  }

  def sessionJarFile(output: File): java.io.File =
    sessionJarFile(output, frames)

  def sessionJarFile(frames: List[Frame]): java.io.File = {
    val path = Files.createTempFile("ammonite-session", ".jar")
    sessionJarFile(path.toFile, frames)
  }

  def sessionJarFile(): java.io.File =
    sessionJarFile(frames)

  def classpath(frames: List[Frame] = frames): Seq[java.io.File] =
    frames.flatMap(_.classpath)

}

// End of ReplAPI
/**
 * Things that are part of the ReplAPI that aren't really "public"
 */
abstract class FullRuntimeAPI extends RuntimeAPI{

  val Internal: Internal
  trait Internal{
    def combinePrints(iters: Iterator[String]*): Iterator[String]

    def print[T: pprint.TPrint: WeakTypeTag]
             (value: => T, ident: String, custom: Option[String])
             (implicit tcolors: pprint.TPrintColors): Iterator[String]

    def printDef(definitionLabel: String, ident: String): Iterator[String]
    def printImport(imported: String): Iterator[String]
  }
  def typeOf[T: WeakTypeTag] = scala.reflect.runtime.universe.weakTypeOf[T]
  def typeOf[T: WeakTypeTag](t: => T) = scala.reflect.runtime.universe.weakTypeOf[T]
  def replArgs: Vector[Bind[_]]
}



trait DefaultRuntimeAPI extends FullRuntimeAPI {

  def help =
    """Welcome to the Ammonite Scala REPL! Enter a Scala expression and it will be evaluated.
      |All your standard Bash hotkeys should work for navigating around or editing the line
      |being entered, as well as some GUI hotkeys like alt-shift-left/right to select words
      |to replace. Hit <tab> to autocomplete possible names.
      |
      |For a list of REPL built-ins and configuration, use `repl.<tab>`. For a more detailed
      |description of how to use the REPL, check out https://lihaoyi.github.io/Ammonite
    """.stripMargin.trim
  object Internal extends Internal{
    def combinePrints(iters: Iterator[String]*) = {
      iters.toIterator
           .filter(_.nonEmpty)
           .flatMap(Iterator(newLine) ++ _)
           .drop(1)
    }

    def print[T: pprint.TPrint: WeakTypeTag](value: => T,
                                             ident: String,
                                             custom: Option[String])
                                            (implicit tcolors: pprint.TPrintColors) = {
      // This type check was originally written as just typeOf[T] =:= typeOf[Unit].
      // However, due to a bug in Scala's reflection when applied to certain
      // class annotations in Hadoop jars, the type check would consistently
      // throw an exception.
      //
      // The solution is to catch exceptions thrown by the typeOf check and fallback
      // to checking the value against Unit's boxed form.
      //
      // Why not just check the value? Because that would force evaluzation of `lazy val`'s
      // which breaks the ammonite.session.EvaluatorTests(lazyvals) test.
      //
      // See https://issues.scala-lang.org/browse/SI-10129 for additional details.
      val isUnit = try {
        typeOf[T] =:= typeOf[Unit]
      } catch {
        case _: Throwable => value == scala.runtime.BoxedUnit.UNIT
      }

      if (isUnit) Iterator()
      else {

        // Pre-compute how many lines and how many columns the prefix of the
        // printed output takes, so we can feed that information into the
        // pretty-printing of the main body
        val prefix = new pprint.Truncated(
          Iterator(
            colors().ident()(ident).render, ": ",
            implicitly[pprint.TPrint[T]].render(tcolors), " = "
          ),
          pprinter().defaultWidth,
          pprinter().defaultHeight
        )
        val output = mutable.Buffer.empty[fansi.Str]

        prefix.foreach(output +=)

        val rhs = custom match {
          case None =>
            pprinter().tokenize(
              value,
              height = pprinter().defaultHeight - prefix.completedLineCount,
              initialOffset = prefix.lastLineLength
            ).toStream
          case Some(s) => Seq(pprinter().colorLiteral(s))
        }

        output.iterator.map(_.render) ++ rhs.map(_.render)
      }
    }
    def printDef(definitionLabel: String, ident: String) = {
      Iterator(
        "defined ", colors().`type`()(definitionLabel).render, " ",
        colors().ident()(ident).render
      )
    }
    def printImport(imported: String) = {
      Iterator(colors().`type`()("import ").render, colors().ident()(imported).render)
    }
  }
}

case class SessionChanged(removedImports: Set[scala.Symbol],
                          addedImports: Set[scala.Symbol],
                          removedJars: Set[java.net.URL],
                          addedJars: Set[java.net.URL])
object SessionChanged{
  def delta(oldFrame: Frame, newFrame: Frame): SessionChanged = {
    def frameSymbols(f: Frame) = f.imports.value.map(_.toName.backticked).map(Symbol(_)).toSet
    new SessionChanged(
      frameSymbols(oldFrame) -- frameSymbols(newFrame),
      frameSymbols(newFrame) -- frameSymbols(oldFrame),
      oldFrame.classloader.allJars.toSet -- newFrame.classloader.allJars.toSet,
      newFrame.classloader.allJars.toSet -- oldFrame.classloader.allJars.toSet
    )
  }
}
