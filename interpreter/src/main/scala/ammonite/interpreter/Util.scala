package ammonite.interpreter

import java.io.{ ByteArrayOutputStream, InputStream }

import fastparse._

import scala.util.Try
import ammonite.api.ImportData
import scalaparse.Scala._

object Res{
  def apply[T](o: Option[T], errMsg: => String) = o match{
    case Some(s) => Success(s)
    case None => Failure(errMsg)
  }
  def apply[T](o: Either[String, T]) = o match{
    case Right(s) => Success(s)
    case Left(t) => Failure(t)
  }
  def apply[T](o: Try[T], errMsg: Throwable => String) = o match{
    case util.Success(s) => Success(s)
    case util.Failure(t) => Failure(errMsg(t))
  }


  /**
   * Successes map and flatmap just like a simple Box[T]
   */
  case class Success[+T](s: T) extends Res[T] {
    def flatMap[V](f: T => Res[V]): Res[V] = f(s)
    def map[V](f: T => V): Res[V] = Success(f(s))
  }

  /**
   * Failing results never call their callbacks, and just remain unchanged
   */
  sealed abstract class Failing extends Res[Nothing]{
    def flatMap[V](f: Nothing => Res[V]): Res[V] = this
    def map[V](f: Nothing => V): Res[V] = this
  }
  case class Failure(s: String) extends Failing
  object Failure{
    def apply(exceptions: Seq[Throwable], stopMethod: String = null, stopClass: String = null): Failure = {
      val traces = exceptions.map(exception =>
        exception.toString + "\n" +
        exception
          .getStackTrace
          .takeWhile(x => x.getMethodName != stopMethod && x.getClassName != stopClass)
          .map("\t" + _)
          .mkString("\n")
      )
      Res.Failure(traces.mkString("\n"))
    }
  }
  case object Skip extends Failing
  case object Exit extends Failing
}

/**
 * The result of a single pass through the ammonite REPL.
 */
sealed abstract class Res[+T]{
  def flatMap[V](f: T => Res[V]): Res[V]
  def map[V](f: T => V): Res[V]
  def filter(f: T => Boolean): Res[T] = this
}

/**
 * Fake for-comprehension generator to catch errors and turn
 * them into [[Res.Failure]]s
 */
case class Catching(handler: PartialFunction[Throwable, Res.Failing]) {

  def foreach[T](t: Unit => T): T = t(())
  def flatMap[T](t: Unit => Res[T]): Res[T] =
    try{t(())} catch handler
  def map[T](t: Unit => T): Res[T] =
    try Res.Success(t(())) catch handler
}

case class Evaluated[T](wrapper: String,
                        imports: Seq[ImportData],
                        value: T)

/**
 * Encapsulates a read-write cell that can be passed around
 */
trait StableRef[T]{
  /**
   * Get the current value of the this [[StableRef]] at this instant in time
   */
  def apply(): T

  /**
   * Set the value of this [[StableRef]] to always be the value `t`
   */
  def update(t: T): Unit
}

trait Ref[T] extends StableRef[T]{
  /**
   * Return a function that can be used to get the value of this [[Ref]]
   * at any point in time
   */
  def live(): () => T

  /**
   * Set the value of this [[Ref]] to always be the value of the by-name
   * argument `t`, at any point in time
   */
  def bind(t: => T): Unit
}

object Ref{
  implicit def refer[T](t: T): Ref[T] = Ref(t)
  // FIXME Move elsewhere
//  implicit def refPPrint[T: PPrint]: PPrinter[Ref[T]] = PPrinter{ (ref, cfg) =>
//    Iterator(cfg.colors.prefixColor, "Ref", cfg.colors.endColor, "(") ++
//      implicitly[PPrint[T]].pprinter.render(ref(), cfg) ++
//      Iterator(")")
//  }
  def live[T](value0: () => T) = new Ref[T]{
    var value: () => T = value0
    def live() = value
    def apply() = value()
    def update(t: T) = value = () => t
    def bind(t: => T): Unit = value = () => t
    override def toString = s"Ref($value)"
  }
  def apply[T](value0: T) = live(() => value0)
}

/**
 * Nice pattern matching for chained exceptions
 */
object Ex{
  def unapplySeq(t: Throwable): Option[Seq[Throwable]] = {
    def rec(t: Throwable): List[Throwable] = {
      t match {
        case null => Nil
        case t => t :: rec(t.getCause)
      }
    }
    Some(rec(t))
  }
}

object Util {
  def transpose[A](xs: List[List[A]]): List[List[A]] = xs.filter(_.nonEmpty) match {
    case Nil    =>  Nil
    case ys: List[List[A]] => ys.map{ _.head }::transpose(ys.map{ _.tail })
  }

  def readFully(is: InputStream) = {
    val buffer = new ByteArrayOutputStream()
    val data = Array.ofDim[Byte](16384)

    var nRead = is.read(data, 0, data.length)
    while (nRead != -1) {
      buffer.write(data, 0, nRead)
      nRead = is.read(data, 0, data.length)
    }

    buffer.flush()
    buffer.toByteArray
  }
}

object Timer{
  var current = 0L
  def reset() = current = System.nanoTime()
  /**
   * Prints the time, in millis, that has passed
   * since the last time `reset` or `apply` was called
   */
  def apply(s: String) = {
    val now = System.nanoTime()
    println(s + ": " + (now - current) / 1000000.0)
    current = now
  }
}

object NamesFor {
  import scala.reflect.runtime.universe._

  val default = typeOf[Object].members
    .flatMap(m => List(m.name.decodedName.toString, m.name.encodedName.toString))
    .toSet

  def apply(t: scala.reflect.runtime.universe.Type): Map[String, Boolean] = {
    val yours = t.members.map(s => s.name.toString -> s.isImplicit).toMap
      .filterKeys(!_.endsWith(nme.LOCAL_SUFFIX_STRING)) // See http://stackoverflow.com/a/17248174/3714539
    yours -- default
  }

  def apply[T: TypeTag]: Map[String, Boolean] = apply(typeOf[T])
}
object SyntaxError{
  def msg(code: String, p: fastparse.core.Parser[_], idx: Int) = {
    val locationString = {
      val (first, last) = code.splitAt(idx)
      val lastSnippet = last.split('\n')(0)
      val firstSnippet = first.reverse.split('\n').lift(0).getOrElse("").reverse
      firstSnippet + lastSnippet + "\n" + (" " * firstSnippet.length) + "^"
    }
    val literal = fastparse.Utils.literalize(code.slice(idx, idx + 20))
    s"SyntaxError: found $literal, expected $p in\n$locationString"
  }
}
class SyntaxError(code: String, p: fastparse.core.Parser[_], idx: Int) extends Exception{
  override def toString() = SyntaxError.msg(code, p, idx)
}
