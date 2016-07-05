package reeds

import cats.data.{Validated, ValidatedNel}, Validated._
import cats.syntax.functor._
import cats.Id

trait Reads[T] extends (String => ValidatedNel[Throwable, T]) {
  def map[U](f: T => U) = Reads.instance[U] {
    str => apply(str).map(f)
  }
}

object Reads extends ReadsInstances {

  def apply[T](str: String)(implicit r: Reads[T]) = r(str)

  case class SimpleError(message: String) extends Exception(message)
  case class WrappedError(message: String, cause: Throwable) extends Exception(message, cause)
  case class ExternalError(cause: Throwable) extends Exception(cause.getMessage, cause)

  implicit class ErrorWrap(val error: Throwable) extends AnyVal {
    def wrap = ExternalError(error)
    def wrap(message: String) = WrappedError(message, error)
  }

  // Create a Reads instance using the given function
  def instance[T](f: String => ValidatedNel[Throwable, T]) = new Reads[T] {
    def apply(s: String) = f(s)
  }

  // Create a Reads instance which `try`s the given function and fails validation on any exception, otherwise succeeds.
  def catching[T](f: String => T) : Reads[T] = Reads.instance[T] { str =>
    Validated.catchNonFatal {
      f(str)
    }.toValidatedNel
  }

}

trait ReadsInstances extends base with net with time with util with math with ReadsInstances0

trait ReadsInstances0 extends ReadsInstances1

trait ReadsInstances1 {
  implicit val stringReads = Reads.instance[String] {
    s => Valid(s)
  }
}
