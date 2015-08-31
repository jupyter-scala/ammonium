package ammonite.interpreter

import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.reflect.internal.util.Position
import scala.reflect.io
import scala.reflect.io._
import scala.tools.nsc
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.interactive.Response

import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.nsc.util._

import ammonite.api.ImportData


/**
 * Encapsulates (almost) all the ickiness of Scalac so it doesn't leak into
 * the rest of the codebase. Makes use of a good amount of mutable state
 * for things like the log-output-forwarder or compiler-plugin-output because
 * These things are hard-coded into Scalac and can't be passed in from run to
 * run.
 *
 * Turns source-strings into the bytes of classfiles, possibly more than one
 * classfile per source-string (e.g. inner classes, or lambdas). Also lets
 * you query source strings using an in-built presentation compiler
 */
trait Compiler {
  /**
   * Compiles a blob of bytes and spits of a list of classfiles
   */
  def compile(src: Array[Byte], runLogger: String => Unit): Compiler.Output
  /**
   * Either the statements that were parsed or the error message
   */
  def parse(line: String): Either[String, Seq[(Global#Tree, Seq[Global#Name])]]

  /**
   * Writes files to dynamicClasspath. Needed for loading cached classes.
   */
  def addToClasspath(classFiles: Compiler.ClassFiles): Unit
}

object Compiler {

  type ClassFiles = Traversable[(String, Array[Byte])]

  /**
   * If the Option is None, it means compilation failed
   * Otherwise it's a Traversable of (filename, bytes) tuples
   */
  type Output = Option[(Traversable[(String, Array[Byte])], Seq[ImportData])]

  /**
   * Converts a bunch of bytes into Scalac's weird VirtualFile class
   */
  def makeFile(src: Array[Byte], name: String = "Main.scala") = {
    val singleFile = new io.VirtualFile(name)
    val output = singleFile.output
    output.write(src)
    output.close()
    singleFile
  }

  /**
   * Converts Scalac's weird Future type
   * into a standard scala.concurrent.Future
   */
  def awaitResponse[T](func: Response[T] => Unit): T = {
    val r = new Response[T]
    func(r)
    r.get.fold(
      x => x,
      e => throw e
    )
  }

  /**
   * Code to initialize random bits and pieces that are needed
   * for the Scala compiler to function, common between the
   * normal and presentation compiler
   */
  def initGlobalBits(
    jarDeps: Seq[java.io.File],
    dirDeps: Seq[java.io.File],
    dynamicClasspath: VirtualDirectory,
    options: List[String],
    logger: => String => Unit,
    errorColor: String
  ) = {
    val vd = new io.VirtualDirectory("(memory)", None)

    val settings = new Settings
    settings.processArguments(options, true)
    settings.Yrangepos.value = true

    val jCtx = new JavaContext()
    val jDirs =
      jarDeps
        .map(jar => new DirectoryClassPath(new FileZipArchive(jar), jCtx))
        .toVector ++
      dirDeps
        .map(dir => new DirectoryClassPath(new PlainDirectory(new Directory(dir)), jCtx)) ++
      Seq(new DirectoryClassPath(dynamicClasspath, jCtx))

    val jcp = new JavaClassPath(jDirs, jCtx)

    settings.outputDirs.setSingleOutput(vd)

    val settings0 = settings

    val reporter =
      new AbstractReporter {
        val settings = settings0
        def displayPrompt(): Unit = ???

        def display(pos: Position, msg: String, severity: Severity) =
          logger(
            severity match {
              case ERROR => errorColor + Position.formatMessage(pos, msg, false) + scala.Console.RESET
              case _     => msg
            }
          )
      }

    (settings, reporter, vd, jcp)
  }

  val PluginXML = "scalac-plugin.xml"

  def pluginClasses(classLoader: ClassLoader) = {
    val urls = classLoader
      .getResources(PluginXML)
      .asScala
      .toVector

    val plugins =
      for {
        url <- urls
        elem = scala.xml.XML.load(url.openStream())
        name = (elem \\ "plugin" \ "name").text
        className = (elem \\ "plugin" \ "classname").text
        if name.nonEmpty && className.nonEmpty
        classOpt =
          try Some(classLoader.loadClass(className))
          catch { case _: ClassNotFoundException => None }
      } yield (name, className, classOpt)

    val notFound = plugins.collect{case (name, className, None) => (name, className) }
    if (notFound.nonEmpty) {
      for ((name, className) <- notFound.sortBy(_._1))
        Console.err.println(s"Implementation $className of plugin $name not found.")
    }

    plugins.collect{case (name, _, Some(cls)) => name -> cls }
  }

  def apply(
    jarDeps: Seq[java.io.File],
    dirDeps: Seq[java.io.File],
    dynamicClasspath: VirtualDirectory,
    options: List[String],
    evalClassLoader: => ClassLoader,
    pluginClassLoader: => ClassLoader,
    shutdownPressy: () => Unit
  ): Compiler = {

    var logger: String => Unit =
      s => ()

    var lastImports = Seq.empty[ImportData]

    val (settings, reporter, vd, jcp) = initGlobalBits(
      jarDeps, dirDeps, dynamicClasspath, options, logger, scala.Console.RED
    )

    def plugins0(g: nsc.Global) = List(new AmmonitePlugin(g, lastImports = _)) ++ {
      for {
        (name, cls) <- pluginClasses(pluginClassLoader)
        plugin = Plugin.instantiate(cls, g)
        initOk =
          try CompilerCompatibility.pluginInit(plugin, Nil, g.globalError)
          catch { case ex: Exception =>
            Console.err.println(s"Warning: disabling plugin $name, initialization failed: $ex")
            false
          }
        if initOk
      } yield plugin
    }

    val scalac: nsc.Global =
      new nsc.Global(settings, reporter) { g =>
        override def classPath =
          platform.classPath // Actually jcp, avoiding a path-dependent type issue in 2.10 here
        override lazy val platform: ThisPlatform =
          new JavaPlatform {
            val global: g.type = g
            override def classPath = jcp
          }

        override lazy val analyzer =
          CompilerCompatibility.analyzer(g, evalClassLoader)
        override lazy val plugins =
          plugins0(g)
      }

    // Initialize scalac to the parser phase immediately, so we can start
    // using Compiler#parse even if we haven't compiled any compilation
    // units yet due to caching
    val run = new scalac.Run()
    scalac.phase = run.parserPhase
    run.cancel()

    def referencedNames(member: scalac.Tree): List[scalac.Name] = {
      val importVars = new mutable.HashSet[scalac.Name]()

      val tvs =
        new scalac.Traverser {
          override def traverse(ast: scalac.Tree) =
            ast match {
              case scalac.Ident(name) =>
                // Comments from scalac (or Spark?) say:
                //   XXX this is obviously inadequate but it's going to require some effort to get right.
                if (!name.toString.startsWith("x$"))
                  importVars += name
              case _ =>
                super.traverse(ast)
            }
        }

      tvs.traverse(member)

      importVars.toList
    }


    new Compiler {
      def compile(src: Array[Byte], runLogger: String => Unit) = {
        scalac.reporter.reset()
        logger = runLogger

        val singleFile = makeFile( src)

        val run = new scalac.Run()
        vd.clear()
        run.compileFiles(List(singleFile))

        if (reporter.hasErrors)
          None
        else {
          shutdownPressy()

          val files =
            for {
              f <- vd.iterator.to[collection.immutable.Traversable]
              if f.name.endsWith(".class")
            } yield {
              val output = dynamicClasspath.fileNamed(f.name).output
              output.write(f.toByteArray)
              output.close()
              (f.name.stripSuffix(".class"), f.toByteArray)
            }

          val imports = lastImports.toList

          Some((files, imports))
        }
      }

      def addToClasspath(classFiles: Traversable[(String, Array[Byte])]) =
        for ((name, bytes) <- classFiles){
          val output = dynamicClasspath.fileNamed(s"$name.class").output
          output.write(bytes)
          output.close()
        }

      def parse(line: String) = {
        val out = mutable.Buffer.empty[String]
        logger = out.append(_)
        reporter.reset()

        val parser = scalac.newUnitParser(line)
        val trees = CompilerCompatibility.trees(scalac)(parser)

        if (reporter.hasErrors)
          Left(out.mkString("\n"))
        else
          Right(
            trees.map(tree =>
              tree -> referencedNames(tree).map(_.decodedName)
            )
          )
      }
    }
  }
}
