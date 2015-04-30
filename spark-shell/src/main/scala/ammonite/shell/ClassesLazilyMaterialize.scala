package ammonite.shell

import java.io.{FileOutputStream, File}
import java.net.URLClassLoader
import java.util.UUID

import ammonite.interpreter.Classes

trait ClassesLazilyMaterialize extends Classes {
  lazy val tmpClassDir = {
    val d = new File(new File(System.getProperty("java.io.tmpdir")), s"ammonite-${UUID.randomUUID()}")
    d.mkdirs()
    d.deleteOnExit()
    d
  }

  var overriddenClassLoader: ClassLoader = _

  abstract override def currentClassLoader: ClassLoader = {
    val _super = super.currentClassLoader
    if (overriddenClassLoader == null || overriddenClassLoader.getParent != _super)
      overriddenClassLoader = new URLClassLoader(Array(), _super) {
        // FIXME getResources should be overloaded too
        override def getResource(name: String) =
          Some(name).filter(_ endsWith ".class").map(_ stripSuffix ".class").flatMap(fromClassMaps) match {
            case Some(bytes) =>
              val f = new File(tmpClassDir, name)
              if (!f.exists()) {
                val w = new FileOutputStream(f)
                w.write(bytes)
                w.close()
              }
              f.toURI.toURL
            case None =>
              super.getResource(name)
          }
      }

    overriddenClassLoader
  }
}
