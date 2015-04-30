package ammonite.pprint

import shapeless._

// Temporary workaround for https://github.com/lihaoyi/Ammonite/issues/47, when using class-based REPL wrappers

trait Shapeless {

  trait Unpacker[T] {
    def apply(t: T, cfg: Config): Iterator[Iterator[String]]
  }
  
  object Unpacker {
    def unpacker[T](f: (T, Config) => Iterator[Iterator[String]]): Unpacker[T] =
      new Unpacker[T] {
        def apply(t: T, cfg: Config) = f(t, cfg)
      }
  }

  implicit val hnilUnpacker: Unpacker[HNil] =
    Unpacker.unpacker((_, c) => Iterator.empty)

  implicit def hconsUnpacker[H, T <: HList](implicit
    headPPrint: PPrint[H],
    tailPPrint: Unpacker[T]
  ): Unpacker[H :: T] =
    Unpacker.unpacker { case (h :: t, cfg) =>
      Iterator(headPPrint.copy(cfg = cfg).render(h)) ++ tailPPrint(t, cfg)
    }

  def fromUnpackerCustom[T](isSingleton: Boolean)(prefix: T => String)(f: Internals.Unpacker[T]): PPrinter[T] = PPrinter[T]{
    (t: T, c: Config) =>
      if (isSingleton && f(t, c).isEmpty)
        Iterator(prefix(t))
      else
        Internals.handleChunks(prefix(t), c, f(t, _))
  }

  implicit def instancePPrint[F, G](implicit
    gen: Generic.Aux[F, G],
    unpacker: Unpacker[G],
    cfg: Config,
    m: Manifest[F]
  ): PPrint[F] = {
    val (name, isSingleton) = {
      var name0 = m.runtimeClass.getName
      var isSingleton0 = false

      if (name0 == "void") name0 = ""

      if (name0 endsWith "$") {
        isSingleton0 = true
        name0 = name0 dropRight 1
      }

      val dolIdx = name0 lastIndexOf '$'
      if (dolIdx >= 0) name0 = name0 drop dolIdx+1

      (name0, isSingleton0)
    }

    val p = PPrint(fromUnpackerCustom(isSingleton)((_: F) => name){ (f, c) => val it = unpacker(gen.to(f), c); it}, cfg)

    if (isSingleton)
      PPrint(p.map(s => if (s endsWith "()") s stripSuffix "()" else s), cfg)
    else
      p
  }

}

object Shapeless extends Shapeless
