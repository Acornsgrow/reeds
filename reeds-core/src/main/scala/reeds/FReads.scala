package reeds

import cats.Functor
import cats.data._

import scala.language.higherKinds

trait FReads[F[_], T] extends (F[String] => F[ValidatedNel[Throwable, T]])

object FReads {

  implicit def derive[F[_], T](implicit reads: Reads[T], functor: Functor[F]) = new FReads[F, T] {
    def apply(f: F[String]) = functor.map(f)(reads(_))
  }


  def apply[F[_], T](f: F[String])(implicit freads: FReads[F, T]) = freads.apply(f)
}