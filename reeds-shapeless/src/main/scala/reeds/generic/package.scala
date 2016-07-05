package reeds

import cats.data.{Validated, ValidatedNel}
import cats.std.list._
import Validated._
import cats.Functor
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.{Comapped, Length, LiftAll, Mapped, RightFolder, ToTraversable, ZipWithKeys}
import shapeless.ops.nat.ToInt
import shapeless.ops.record.{Keys, Values}
import shapeless.ops.traversable.FromTraversable

import scala.collection.GenTraversable
import scala.language.higherKinds

package object generic {

  case class WrongArity(expected: Int, actual: Int) extends
    Throwable(s"Wrong number of elements; expected $expected, but there were $actual")

  case class MissingField(name: String) extends Throwable(s"Missing non-optional field '$name'")

  case class WrongFieldArity(name: String, expected: Int, actual: Int) extends
    Throwable(s"Wrong number of elements for field '$name'; expected $expected, but there were $actual")

  object Sequencer extends Poly2 {
    implicit def valid[T, U <: HList, X] : Case.Aux[ValidatedNel[X, T], ValidatedNel[X, U], ValidatedNel[X, T :: U]] =
      at[ValidatedNel[X, T], ValidatedNel[X, U]] {
        case (Valid(t), Valid(u)) => Valid(t :: u)
        case (Valid(t), i @ Invalid(x)) => i
        case (i @ Invalid(x), Valid(u)) => i
        case (Invalid(x1), Invalid(x2)) => Invalid(x1 combine x2)
      }
  }

  object FieldSequencer extends Poly2 {
    implicit def valid[K <: Symbol, V, T <: HList] : Case.Aux[FieldType[K, ValidatedNel[Throwable, V]], ValidatedNel[Throwable, T], ValidatedNel[Throwable, FieldType[K, V] :: T]] =
      at[FieldType[K, ValidatedNel[Throwable, V]], ValidatedNel[Throwable, T]] {
        (head, tail) => (head.asInstanceOf[ValidatedNel[Throwable, V]],tail) match {
          case (Valid(v), Valid(t)) => Valid(field[K].apply(v) :: t)
          case (Valid(t), i@Invalid(x)) => i
          case (i@Invalid(x), Valid(u)) => i
          case (Invalid(x1), Invalid(x2)) => Invalid(x1 combine x2)
        }
      }
  }


  implicit class ReedsShapelessStringFunctorOps[F[A] <: GenTraversable[A]](val seq: F[String]) extends AnyVal {
    def read[T](implicit reads: GenericFunctorReads[F, T]) = reads.apply(seq)
  }

  implicit class ReedsShapelessStringMapOps(val map: Map[String, String]) extends AnyVal {
    def readMap[T](implicit reads: GenericMapReads[T]) = reads.apply(map)
  }

  implicit class ReedsShapelessStringFMapOps[F[A] <: GenTraversable[A]](val map: Map[String, F[String]]) extends AnyVal {
    def readMap[T](implicit reads: GenericFMapReads[F, T]) = reads.apply(map)
  }

}
