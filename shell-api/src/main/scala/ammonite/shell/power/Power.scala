package ammonite.shell.power

import java.io.File

import ammonite.interpreter.{Decl, ImportData}

/*
 * TODO
 * Move Power, ..., to a interpreter-api project, and add it as a dependency to interpreter
 * to avoid code duplication in interpreter itself
 */

trait Power {
  /**
   *
   */
  def classes: Classes

  /**
   * Throw away the current scala.tools.nsc.Global and get a new one
   */
  def newCompiler(): Unit

  /**
   *
   */
  def imports: Imports

  /**
   *
   */
  def decls(code: String): Either[String, Seq[Decl]]

  /**
   *
   */
  def wrap(code: String): Either[String, String]

  /**
   *
   */
  def compile(code: String): (String, Option[(Traversable[(String, Array[Byte])], Seq[ImportData])])

  /**
   *
   */
  def onStop(action: => Unit): Unit

  /**
   *
   */
  def stop(): Unit

  def complete(snippetIndex: Int, snippet: String): (Int, Seq[String], Seq[String])
}

trait Classes {
  def currentClassLoader: ClassLoader
  def jars: Seq[File]
  def dirs: Seq[File]
  def addJar(jar: File): Unit
  def onJarsAdded(action: Seq[File] => Unit): Unit
  def fromAddedClasses(name: String): Option[Array[Byte]]

  def underlying: AnyRef
}

trait Imports {
  def previousImportBlock(wanted: Option[Set[String]] = None): String
  def update(newImports: Seq[ImportData]): Unit
}
