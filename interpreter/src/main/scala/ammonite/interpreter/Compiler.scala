package ammonite.interpreter


import acyclic.file
import scala.collection.mutable
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
trait Compiler{
  def compile(src: Array[Byte], runLogger: String => Unit): Compiler.Output

  /**
   * Either the statements that were parsed or the error message
   */
  def parse(line: String): Either[String, Seq[(Global#Tree, Seq[Global#Name])]]

  def plugins: Seq[(String, Class[_])]
}
object Compiler{
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
  def initGlobalBits(jarDeps: Seq[java.io.File],
                     dirDeps: Seq[java.io.File],
                     dynamicClasspath: VirtualDirectory,
                     options: List[String],
                     logger: => String => Unit,
                     errorColor: String)= {
    val vd = new io.VirtualDirectory("(memory)", None)
    lazy val settings = new Settings
    settings.processArguments(options, true)
    val settingsX = settings
    settingsX.Yrangepos.value = true
    val jCtx = new JavaContext()
    val jDirs = jarDeps.map(x =>
      new DirectoryClassPath(new FileZipArchive(x), jCtx)
    ).toVector ++ dirDeps.map(x =>
      new DirectoryClassPath(new PlainDirectory(new Directory(x)), jCtx)
    ) ++ Seq(new DirectoryClassPath(dynamicClasspath, jCtx))
    val jcp = new JavaClassPath(jDirs, jCtx)
    settings.outputDirs.setSingleOutput(vd)

    val reporter = new AbstractReporter {
      def displayPrompt(): Unit = ???

      def display(pos: Position, msg: String, severity: Severity) = {
        severity match{
          case ERROR => logger(
            errorColor + Position.formatMessage(pos, msg, false) + scala.Console.RESET
          )
          case _ => logger(msg)
        }
      }

      val settings = settingsX
    }
    (settings, reporter, vd, jcp)
  }

  def apply(jarDeps: Seq[java.io.File],
            dirDeps: Seq[java.io.File],
            dynamicClasspath: VirtualDirectory,
            options: List[String],
            evalClassloader: => ClassLoader,
            shutdownPressy: () => Unit,
            enableCompilerPlugins: Boolean = true): Compiler = new Compiler { self =>

    val PluginXML = "scalac-plugin.xml"

    lazy val plugins = if (enableCompilerPlugins) {
      import scala.collection.JavaConverters._
      val loader = evalClassloader

      val plugins =
        loader.getResources(PluginXML).asScala
          .toList
          .map(url => scala.xml.XML.load(url.openStream()))
          .map { el =>
            val name = (el \\ "plugin" \ "name").text
            val className = (el \\ "plugin" \ "classname").text
            name -> className
          }
          .filter{ case (n, cn) => n.nonEmpty && cn.nonEmpty }
          .map { case (name, className) =>
            name -> (className,
                try Some(loader.loadClass(className))
                catch { case _: ClassNotFoundException => None })
          }

      val (found, notFound) = plugins.partition(_._2._2.nonEmpty)
      if (notFound.nonEmpty) {
        for ((name, (className, _)) <- notFound.sortBy(_._1))
          Console.err.println(s"Implementation $className of plugin $name not found.")
      }

      found.map{ case (name, (_, Some(cls))) => name -> cls }
    } else Nil

    var logger: String => Unit = s => ()

    var lastImports = Seq.empty[ImportData]

    val (vd, reporter, compiler) = {
      val (settings, reporter, vd, jcp) = initGlobalBits(
        jarDeps, dirDeps, dynamicClasspath, options, logger, scala.Console.RED
      )
      val scalac = new nsc.Global(settings, reporter) { g =>
        override lazy val plugins =
          new AmmonitePlugin(g, lastImports = _) ::
            self.plugins
              .map{case (_, cls) => Plugin.instantiate(cls, g) }
              .filter{ plg => CompilerCompatibility.pluginInit(plg, Nil, g.globalError) }
        override def classPath = platform.classPath // Actually jcp, avoiding a path-dependent type issue in 2.10 here
        override lazy val platform: ThisPlatform = new JavaPlatform{
          val global: g.type = g
          override def classPath = jcp
        }
        override lazy val analyzer = CompilerCompatibility.analyzer(g, evalClassloader)
      }
      (vd, reporter, scalac)
    }

    /**
     * Compiles a blob of bytes and spits of a list of classfiles
     */
    def compile(src: Array[Byte], runLogger: String => Unit): Output = {
      compiler.reporter.reset()
      this.logger = runLogger
      val singleFile = makeFile( src)

      val run = new compiler.Run()
      vd.clear()
      run.compileFiles(List(singleFile))
      if (reporter.hasErrors) None
      else Some{
        shutdownPressy()

        val files = for{
          x <- vd.iterator.to[collection.immutable.Traversable]
          if x.name.endsWith(".class")
        } yield {
          val output = dynamicClasspath.fileNamed(x.name).output
          output.write(x.toByteArray)
          output.close()
          (x.name.stripSuffix(".class"), x.toByteArray)
        }
        val imports = lastImports.toList
        (files, imports)
      }
    }

    def referencedNames(member: compiler.Tree): List[compiler.Name] = {
      val importVars = new scala.collection.mutable.HashSet[compiler.Name]()

      val tvs =
        new compiler.Traverser {
          override def traverse(ast: compiler.Tree) = ast match {
            case compiler.Ident(name) =>
              // Comments from scalac (or Spark?) say:
              //   XXX this is obviously inadequate but it's going to require some effort to get right.
              if (!name.toString.startsWith("x$"))
                importVars += name
            case _ =>
              super.traverse(ast)
          }
        }
      tvs traverse member

      importVars.toList
    }


    def parse(line: String): Either[String, Seq[(Global#Tree, Seq[Global#Name])]] = {
      val out = mutable.Buffer.empty[String]
      logger = out.append(_)
      reporter.reset()
      val parser = compiler.newUnitParser(line)
      val trees = CompilerCompatibility.trees(compiler)(parser)
      if (reporter.hasErrors) Left(out.mkString("\n"))
      else Right(trees.map(tree => tree -> referencedNames(tree).map(_.decodedName)))
    }
  }
}
