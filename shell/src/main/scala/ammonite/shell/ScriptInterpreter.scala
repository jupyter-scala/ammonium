package ammonite.shell

import java.security.MessageDigest

import ammonite.api.ImportData
import ammonite.interpreter.Compiler.ClassFiles
import ammonite.interpreter.{Timer, Res, Interpreter}
import ammonite.shell.ScriptInterpreter.CompileCache

object ScriptInterpreter {

  type CompileCache = (ClassFiles, Seq[ImportData])

}

class ScriptInterpreter(
  interpreter: Interpreter,
  cacheLoad: String => Option[CompileCache],
  cacheSave: (String, CompileCache) => Unit,
  addToCompilerClasspath: ClassFiles => Unit
) {

  def md5Hash(data: Array[Byte]) = MessageDigest.getInstance("MD5").digest(data)

  /**
   * This gives our cache tags for compile caching. The cache tags are a hash
   * of classpath, previous commands (in-same-script), and the block-code.
   * Previous commands are hashed in the wrapper names, which are contained
   * in imports, so we don't need to pass them explicitly.
   */
  def cacheTag(code: String, imports: Seq[ImportData], classpathHash: Array[Byte]): String = {
    val bytes = md5Hash(md5Hash(code.getBytes) ++ md5Hash(imports.mkString.getBytes) ++ classpathHash)
    "cache" + bytes.map("%02x".format(_)).mkString //add prefix to make sure it begins with a letter
  }

  def cachedCompileBlock(code: String,
                         imports: Seq[ImportData],
                         printCode: String = ""): Res[(String, Class[_], Seq[ImportData])] = {
    Timer("cachedCompileBlock 0")
    val wrapperName = cacheTag(code, imports, ??? /* interpreter.classes.currentClassLoader.classpathHash */)

    Timer("cachedCompileBlock 2")
    val compiled = cacheLoad(wrapperName) match {
      case Some((classFiles, newImports)) =>
        addToCompilerClasspath(classFiles)
        Res.Success((classFiles, newImports))
      case None => for {
        (classFiles, newImports) <- interpreter.compile(code)
        _ = cacheSave(wrapperName, (classFiles, newImports))
      } yield (classFiles, newImports)
    }

    Timer("cachedCompileBlock 3")
    for {
      (classFiles, newImports) <- compiled
      _ = Timer("cachedCompileBlock 4")
      cls <- interpreter.loadClass(wrapperName, classFiles)
    } yield (wrapperName, cls, newImports)
  }

  def processScriptBlock(code: String, scriptImports: Seq[ImportData]) =
    for ((wrapperName, cls, newImports) <- cachedCompileBlock(code, scriptImports)) yield {
      Timer("cachedCompileBlock")
      interpreter.evalMain(cls)
      Timer("evalMain")
      val res = interpreter.evaluationResult(wrapperName, newImports, ???)
      Timer("evaluationResult")
      res
    }
}
