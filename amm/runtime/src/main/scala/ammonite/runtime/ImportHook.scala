package ammonite.runtime

import java.io.File

import acyclic.file
import ammonite.ops.{read, _}
import ammonite.runtime.tools.DependencyThing
import ammonite.util.Util.CodeSource
import ammonite.util._

/**
  * An extensible hook into the Ammonite REPL's import system; allows the end
  * user to hook into `import $foo.bar.{baz, qux => qua}` syntax, and in
  * response load jars or process source files before the "current" compilation
  * unit is run. Can be used to load script files, ivy dependencies, jars, or
  * files from the web.
  */
trait ImportHook{
  /**
    * Handle a parsed import that this import hook was registered to be interested in
    *
    * Note that `source` is optional; not every piece of code has a source. Most *user*
    * code does, e.g. a repl session is based in their CWD, a script has a path, but
    * some things like hardcoded builtin predefs don't
    */
  def handle(source: Option[Path],
             tree: ImportTree,
             interp: ImportHook.InterpreterInterface)
            : Either[String, Seq[ImportHook.Result]]
}

object ImportHook{

  /**
    * The minimal interface that is exposed to the import hooks from the
    * Interpreter. Open for extension, if someone needs more stuff, but by
    * default this is what is available.
    */
  trait InterpreterInterface{
    def wd: Path
    def exclude(coordinates: (String, String)*): Unit
    def addProfile(profile: String): Unit
    def addRepository(repository: String): Unit
    def profiles: Set[String]
    def addedDependencies(plugin: Boolean): Seq[(String, String, String)]
    def exclusions(plugin: Boolean): Seq[(String, String)]
    def loadIvy(
      coordinates: Seq[(String, String, String)],
      previousCoordinates: Seq[(String, String, String)],
      exclusions: Seq[(String, String)]
    ): Set[File]
  }

  /**
    * The result of processing an [[ImportHook]]. Can be either a source-file
    * to evaluate, or additional files/folders/jars to put on the classpath
    */
  sealed trait Result
  object Result{
    case class Source(code: String,
                      blockInfo: CodeSource,
                      imports: Imports,
                      exec: Boolean) extends Result
    case class ClassPath(files: Seq[Path], coordinates: Seq[(String, String, String)], plugin: Boolean) extends Result
  }

  
  object File extends SourceHook(false)
  object Exec extends SourceHook(true)

  def resolveFiles(tree: ImportTree, currentScriptPath: Path, extensions: Seq[String])
                  : (Seq[(RelPath, Option[String])], Seq[Path], Seq[Path]) = {
    val relative =
      tree.prefix
        .map{case ammonite.util.Util.upPathSegment => up; case x => ammonite.ops.empty/x}
        .reduce(_/_)
    val relativeModules = tree.mappings match{
      case None => Seq(relative -> None)
      case Some(mappings) => for((k, v) <- mappings) yield relative/k -> v
    }
    def relToFile(x: RelPath) = {
      val base = currentScriptPath/up/x/up/x.last
      extensions.find(ext => exists! base/up/(x.last + ext)) match{
        case Some(p) => Right(base/up/(x.last + p): Path)
        case None => Left(base)
      }

    }
    val resolved = relativeModules.map(x => relToFile(x._1))
    val missing = resolved.collect{case Left(p) => p}
    val files = resolved.collect{case Right(p) => p}
    (relativeModules, files, missing)
  }
  class SourceHook(exec: Boolean) extends ImportHook {
    // import $file.foo.Bar, to import the file `foo/Bar.sc`
    def handle(source: Option[Path], 
               tree: ImportTree, 
               interp: InterpreterInterface) = {

      source match{
        case None => Left("Cannot resolve $file import in code without source")
        case Some(currentScriptPath) =>

          val (relativeModules, files, missing) = resolveFiles(
            tree, currentScriptPath, Seq(".sc")
          )

          if (missing.nonEmpty) Left("Cannot resolve $file import: " + missing.mkString(", "))
          else {
            Right(
              for(((relativeModule, rename), filePath) <- relativeModules.zip(files)) yield {
                val (pkg, wrapper) = Util.pathToPackageWrapper(filePath, interp.wd)
                val fullPrefix = pkg ++ Seq(wrapper)

                val importData = Seq(ImportData(
                  Name("wrapper"), Name(rename.getOrElse(relativeModule.last)),
                  fullPrefix :+ Name("wrapper"), ImportData.TermType
                ))

                if (sys.env.contains("DEBUG") || sys.props.contains("DEBUG")) println(s"Source import data:\n${importData.mkString("\n")}\n")

                Result.Source(
                  Util.normalizeNewlines(read(filePath)),
                  CodeSource(wrapper, pkg, Some(filePath)),
                  Imports(importData),
                  exec
                )
              }
            )
          }
      }

    }
  }

  object Ivy extends BaseIvy(plugin = false)
  object PluginIvy extends BaseIvy(plugin = true)
  object IvyExclude extends BaseExcludeIvy(plugin = false)
  object PluginIvyExclude extends BaseExcludeIvy(plugin = true)
  class BaseIvy(plugin: Boolean) extends ImportHook{
    def name = "$ivy"
    def splitImportTree(tree: ImportTree): Either[String, Seq[String]] = {
      tree match{
        case ImportTree(Seq(part), None, _, _) => Right(Seq(part))
        case ImportTree(Nil, Some(mapping), _, _) if mapping.map(_._2).forall(_.isEmpty) =>
          Right(mapping.map(_._1))
        case _ => Left(s"Invalid $name import " + tree)
      }
    }
    def resolve(interp: InterpreterInterface, signatures: Seq[String]) = {
      val splitted = for (signature <- signatures) yield {
        signature.split(':') match{
          case Array(a, b, c) => Right((a, b, c))
          case Array(a, "", b, c) => Right((a, b + "_" + DependencyThing.scalaBinaryVersion, c))
          case _ => Left(signature)
        }
      }
      val errors = splitted.collect{case Left(error) => error}
      val successes = splitted.collect{case Right(v) => v}
      if (errors.nonEmpty)
        Left("Invalid $ivy imports: " + errors.map(Util.newLine + "  " + _).mkString)
      else
        try Right((interp.loadIvy(successes, interp.addedDependencies(plugin), interp.exclusions(plugin)), successes))
        catch {
          case ex: Exception => Left(ex.toString)
        }
    }


    def handle(source: Option[Path], 
               tree: ImportTree, 
               interp: InterpreterInterface) = {
      // Avoid for comprehension, which doesn't work in Scala 2.10/2.11
      splitImportTree(tree) match{
        case Right(signatures) => resolve(interp, signatures) match{
          case Right((files, coords)) =>
            Right(Seq(Result.ClassPath(files.toSeq.map(Path(_)), coords, plugin)))
          case Left(l) => Left(l)
        }
      }
    }
  }
  class BaseExcludeIvy(plugin: Boolean) extends BaseIvy(plugin){
    override def name = "$exclude"
    override def resolve(interp: InterpreterInterface, signatures: Seq[String]) = {
      val splitted = for (signature <- signatures) yield {
        signature.split(':') match{
          case Array(a, b) => Right((a, b))
          case Array(a, "", b) => Right((a, b + "_" + DependencyThing.scalaBinaryVersion))
          case _ => Left(signature)
        }
      }
      val errors = splitted.collect{case Left(error) => error}
      val successes = splitted.collect{case Right(v) => v}
      if (errors.nonEmpty) Left("Invalid $ivy imports: " + errors.map("\n\t" + _).mkString)
      else
        try {
          interp.exclude(successes: _*)
          Right((Set(), Seq()))
        }
        catch {
          case ex: Exception => Left(ex.toString)
        }
    }
  }
  object MavenProfile extends BaseIvy(plugin = false){
    override def name = "$profile"
    override def resolve(interp: InterpreterInterface, signatures: Seq[String]) = {
      for (signature <- signatures)
        interp.addProfile(signature)
      Right((Set(), Seq()))
    }
  }
  object Repository extends BaseIvy(plugin = false){
    override def name = "$repo"
    override def resolve(interp: InterpreterInterface, signatures: Seq[String]) = {
      for (signature <- signatures)
        interp.addRepository(signature)
      Right((Set(), Seq()))
    }
  }
  object Classpath extends BaseClasspath(plugin = false)
  object PluginClasspath extends BaseClasspath(plugin = true)
  class BaseClasspath(plugin: Boolean) extends ImportHook{
    def handle(source: Option[Path], 
               tree: ImportTree, 
               interp: InterpreterInterface) = {
      source match{
        case None => Left("Cannot resolve $cp import in code without source")
        case Some(currentScriptPath) =>
          val (relativeModules, files, missing) = resolveFiles(
            tree, currentScriptPath, Seq(".jar", "")
          )

          if (missing.nonEmpty) Left("Cannot resolve $cp import: " + missing.mkString(", "))
          else Right(
            for(((relativeModule, rename), filePath) <- relativeModules.zip(files))
              yield Result.ClassPath(Seq(filePath), Nil, plugin)
          )
      }

    }
  }
}
