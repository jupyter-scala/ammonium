package ammonite.interpreter.cl

// Extracted from SBT

/*
 * For later use, to expose a cleaner ClassLoader to users
 */

import java.io.{InputStream, ByteArrayInputStream, File}
import java.io.File._
import java.net._
import java.nio.file.Files
import java.util.Enumeration

import scala.annotation.tailrec

/** Concatenates `a` and `b` into a single `Enumeration`.*/
final class DualEnumeration[T](a: Enumeration[T], b: Enumeration[T]) extends Enumeration[T] {
  // invariant: current.hasMoreElements or current eq b
  private[this] var current = if (a.hasMoreElements) a else b
  def hasMoreElements = current.hasMoreElements
  def nextElement =
  {
    val element = current.nextElement
    if (!current.hasMoreElements)
      current = b
    element
  }
}

/** A ClassLoader that looks up resource requests in a `Map` prior to the base ClassLoader's resource lookups. */
trait FixedResources extends ClassLoader {
  /** The map from resource paths to URL to provide in [[findResource]] and [[findResources]]. */
  protected def resourceURL: Map[String, URL]

  override def findResource(s: String): URL = resourceURL.getOrElse(s, super.findResource(s))

  import java.util.Collections.{ enumeration, singletonList }
  override def findResources(s: String): Enumeration[URL] =
  {
    val sup = super.findResources(s)
    resourceURL.get(s) match {
      case Some(url) => new DualEnumeration(enumeration(singletonList(url)), sup)
      case None      => sup
    }
  }
}

object RawURL {

  /**
   * Constructs a URL with scheme `raw` and path `file` that will return the bytes for `value` in the platform default encoding
   * when a connection to the URL is opened.
   */
  def apply(file: String, value: String): URL =
    apply(file, value.getBytes)

  /** Constructs a URL with scheme `raw` and path `file` that will return the bytes `value` when a connection to the URL is opened. */
  def apply(file: String, value: Array[Byte]): URL =
    apply(file)(new ByteArrayInputStream(value))

  /**
   * Constructs a URL with scheme `raw` and path `file` that will use `value` to construct the `InputStream` used when a connection
   * to the URL is opened.
   */
  def apply(file: String)(value: => InputStream): URL =
    new URL("raw", null, -1, file, new RawStreamHandler(value))


  private[this] final class RawStreamHandler(value: => InputStream) extends URLStreamHandler {
    override protected def openConnection(url: URL, p: Proxy): URLConnection =
      openConnection(url)
    override protected def openConnection(url: URL): URLConnection =
      new URLConnection(url) {
        private lazy val in = value
        def connect() { in }
        override def getInputStream = in
      }
  }
}

/** A ClassLoader that looks up resource requests in a `Map` prior to the base ClassLoader's resource lookups. */
trait RawResources extends FixedResources {
  /** The map from resource paths to the raw String content to provide via the URL returned by [[findResource]] or [[findResources]]. */
  protected def resources: Map[String, String]
  override protected final val resourceURL = resources.transform(RawURL.apply)
}

/**
 * Configures a [[NativeCopyLoader]].
 * The loader will provide native libraries listed in `explicitLibraries` and on `searchPaths` by copying them to `tempDirectory`.
 * If `tempDirectory` is unique to the class loader, this ensures that the class loader gets a unique path for
 * the native library and avoids the restriction on a native library being loaded by a single class loader.
 */
final class NativeCopyConfig(val tempDirectory: File, val explicitLibraries: Seq[File], val searchPaths: Seq[File])

/**
 * Loads native libraries from a temporary location in order to work around the jvm native library uniqueness restriction.
 * See [[NativeCopyConfig]] for configuration details.
 */
trait NativeCopyLoader extends ClassLoader {
  /** Configures this loader.  See [[NativeCopyConfig]] for details. */
  protected val config: NativeCopyConfig
  import config._

  private[this] val mapped = new collection.mutable.HashMap[String, String]

  override protected def findLibrary(name: String): String =
    synchronized { mapped.getOrElseUpdate(name, findLibrary0(name)) }

  private[this] def findLibrary0(name: String): String =
  {
    val mappedName = System.mapLibraryName(name)
    val explicit = explicitLibraries.filter(_.getName == mappedName).toStream
    val search = searchPaths.toStream flatMap relativeLibrary(mappedName)
    (explicit ++ search).headOption.map(copy).orNull
  }
  private[this] def relativeLibrary(mappedName: String)(base: File): Seq[File] =
  {
    val f = new File(base, mappedName)
    if (f.isFile) f :: Nil else Nil
  }
  private[this] def copy(f: File): String =
  {
    val target = new File(tempDirectory, f.getName)
    Files.copy(f.toPath, target.toPath)
    target.getAbsolutePath
  }
}

object ClassLoaderUtil {

  def makeString(paths: Seq[File]): String = makeString(paths, pathSeparator)
  def makeString(paths: Seq[File], sep: String): String = {
    val separated = paths.map(_.getAbsolutePath)
    separated.find(_ contains sep).foreach(p => sys.error(s"Path '$p' contains separator '$sep'"))
    separated.mkString(sep)
  }

  final val AppClassPath = "app.class.path"
  final val BootClassPath = "boot.class.path"

  def toURLs(files: Seq[File]): Array[URL] = files.map(_.toURI.toURL).toArray

  /** A pattern used to split a String by path separator characters.*/
  private val PathSeparatorPattern = java.util.regex.Pattern.compile(File.pathSeparator)

  /** Splits a String around the platform's path separator characters. */
  def pathSplit(s: String) = PathSeparatorPattern.split(s)

  /** Parses a classpath String into File entries according to the current platform's path separator.*/
  def parseClasspath(s: String): Seq[File] = pathSplit(s).map(new File(_)).toSeq

  def javaLibraryPaths: Seq[File] = parseClasspath(System.getProperty("java.library.path"))

  def toLoader(paths: Seq[File], parent: ClassLoader, resourceMap: Map[String, String], nativeTemp: File): ClassLoader =
    new URLClassLoader(toURLs(paths), parent) with RawResources with NativeCopyLoader {
      override def resources = resourceMap
      override val config = new NativeCopyConfig(nativeTemp, paths, javaLibraryPaths)
      override def toString =
        s"""|URLClassLoader with NativeCopyLoader with RawResources(
            |  urls = $paths,
            |  parent = $parent,
            |  resourceMap = ${resourceMap.keySet},
            |  nativeTemp = $nativeTemp
            |)""".stripMargin
    }

  def createClasspathResources(appPaths: Seq[File], bootPaths: Seq[File]): Map[String, String] =
  {
    def make(name: String, paths: Seq[File]) = name -> makeString(paths)
    Map(make(AppClassPath, appPaths), make(BootClassPath, bootPaths))
  }

  def filterByClasspath(classpath: Seq[File], rootLoader: ClassLoader, loader: ClassLoader): ClassLoader =
    new ClasspathFilter(loader, rootLoader, classpath.toSet)

  def makeLoader(classpath: Seq[File], rootLoader: ClassLoader, loader: ClassLoader, bootPath: Seq[File], nativeTemp: File): ClassLoader =
    filterByClasspath(classpath, rootLoader, toLoader(classpath, loader, createClasspathResources(classpath, bootPath), nativeTemp))

  private val FileScheme = "file"

  def toFile(url: URL): File =
    try { new File(url.toURI) }
    catch { case _: URISyntaxException => new File(url.getPath) }

  def uriToFile(uriString: String): File =
  {
    val uri = new URI(uriString)
    assert(uri.getScheme == FileScheme, "Expected protocol to be '" + FileScheme + "' in URI " + uri)
    if (uri.getAuthority eq null)
      new File(uri)
    else {
      /* https://github.com/sbt/sbt/issues/564
    * http://blogs.msdn.com/b/ie/archive/2006/12/06/file-uris-in-windows.aspx
    * http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=5086147
    * The specific problem here is that `uri` will have a defined authority component for UNC names like //foo/bar/some/path.jar
    * but the File constructor requires URIs with an undefined authority component.
    */
      new File(uri.getSchemeSpecificPart)
    }
  }

  def urlAsFile(url: URL): Option[File] =
    url.getProtocol match {
      case FileScheme => Some(toFile(url))
      case "jar" =>
        val path = url.getPath
        val end = path.indexOf('!')
        Some(uriToFile(if (end == -1) path else path.substring(0, end)))
      case _ => None
    }

  private def baseFileString(baseFile: File): Option[String] =
  {
    if (baseFile.isDirectory) {
      val cp = baseFile.getAbsolutePath
      assert(cp.length > 0)
      val normalized = if (cp.charAt(cp.length - 1) == File.separatorChar) cp else cp + File.separatorChar
      Some(normalized)
    } else
      None
  }

  def relativize(base: File, file: File): Option[String] =
  {
    val pathString = file.getAbsolutePath
    baseFileString(base) flatMap
      {
        baseString =>
        {
          if (pathString.startsWith(baseString))
            Some(pathString.substring(baseString.length))
          else
            None
        }
      }
  }

  /** Doesn't load any classes itself, but instead verifies that all classes loaded through `parent` either come from `root` or `classpath`.*/
  final class ClasspathFilter(parent: ClassLoader, root: ClassLoader, classpath: Set[File]) extends ClassLoader(parent) {
    override def toString =
      s"""|ClasspathFilter(
         |  parent = $parent
          |  root = $root
          |  cp = $classpath
          |)""".stripMargin

    private[this] val directories: Seq[File] = classpath.toSeq.filter(_.isDirectory)
    override def loadClass(className: String, resolve: Boolean): Class[_] =
    {
      val c = super.loadClass(className, resolve)
      if (includeLoader(c.getClassLoader, root) || fromClasspath(c))
        c
      else
        throw new ClassNotFoundException(className)
    }
    private[this] def fromClasspath(c: Class[_]): Boolean =
    {
      val codeSource = c.getProtectionDomain.getCodeSource
      (codeSource eq null) ||
        onClasspath(codeSource.getLocation)
    }
    private[this] def onClasspath(src: URL): Boolean =
      (src eq null) || (
        urlAsFile(src) match {
          case Some(f) => classpath(f) || directories.exists(dir => relativize(dir, f).isDefined)
          case None    => false
        }
        )

    override def getResource(name: String): URL = {
      val u = super.getResource(name)
      if (onClasspath(u)) u else null
    }

    override def getResources(name: String): java.util.Enumeration[URL] =
    {
      import collection.convert.WrapAsScala.{ enumerationAsScalaIterator => asIt }
      import collection.convert.WrapAsJava.{ asJavaEnumeration => asEn }
      val us = super.getResources(name)
      if (us ne null) asEn(asIt(us).filter(onClasspath)) else null
    }

    @tailrec private[this] def includeLoader(c: ClassLoader, base: ClassLoader): Boolean =
      (base ne null) && (
        (c eq base) || includeLoader(c, base.getParent)
        )
  }


}
