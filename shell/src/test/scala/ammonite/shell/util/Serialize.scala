package ammonite.shell.util

import java.io._

object Serialize {
  def to(m: AnyRef): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(m)
    oos.close()
    baos.toByteArray
  }

  def from(b: Array[Byte], loader: ClassLoader): AnyRef = {
    val bais = new ByteArrayInputStream(b)
    val ois = new ClassLoaderObjectInputStream(loader, bais)
    val m = ois.readObject()
    ois.close()
    m
  }
}

object Test {

  println("*** decl ***"); val a = { println("*** field ***"); Option(2) }; def f = { println("*** func ***"); Option(2) }; class C extends Serializable { override def toString = "C"; def get = f; def getField = a }

  val c = new C

  val b = ammonite.shell.util.Serialize.to(c)

  val loader = ??? : ClassLoader // interpreter.classes.classLoaderClone

  val c2 = ammonite.shell.util.Serialize.from(b, loader)


  c2.getClass.getMethod("get").invoke(c2)

}