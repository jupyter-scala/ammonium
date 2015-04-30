package ammonite.pprint

import shapeless._

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

  implicit def instancePPrint[F, G](implicit
    gen: Generic.Aux[F, G],
    unpacker: Unpacker[G],
    cfg: Config,
    m: Manifest[F]
  ): PPrint[F] = {
    val name = {
      var name0 = m.runtimeClass.getName

      if (name0 == "void") name0 = ""

      val dolIdx = name0.lastIndexOf('$')
      if (dolIdx >= 0) name0 = name0.drop(dolIdx + 1)

      name0
    }

    PPrint(Internals.fromUnpacker((_: F) => name)((f, c) => unpacker(gen.to(f), c)), cfg)
  }

}

object Shapeless extends Shapeless
