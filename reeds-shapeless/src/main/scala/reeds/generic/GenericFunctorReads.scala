package reeds.generic

import cats.data.Validated.Valid
import cats.data.{Validated, ValidatedNel}
import shapeless.ops.hlist.{Length, RightFolder}
import shapeless.ops.nat.ToInt
import shapeless.ops.traversable.FromTraversable
import shapeless.{Generic, HList, HNil, Nat}

import scala.collection.GenTraversable


trait GenericFunctorReads[F[A] <: GenTraversable[A], T] {
  def apply(in: F[String]) : ValidatedNel[Throwable, T]
}

object GenericFunctorReads {

  implicit def derive[F[A] <: GenTraversable[A], T, In <: HList, Out <: HList, L <: HList, N <: Nat](implicit
    gen: Generic.Aux[T, L],
    readsAll: ReadsAll.AuxInOut[L, In, Out],
    length: Length.Aux[L, N],
    toInt: ToInt[N],
    fromTraversable: FromTraversable[In],
    folder: RightFolder.Aux[Out, ValidatedNel[Throwable, HNil], Sequencer.type, ValidatedNel[Throwable, L]]
  ) : GenericFunctorReads[F, T] = new GenericFunctorReads[F, T] {

    def apply(in: F[String]) = {
      val opt = fromTraversable.apply(in) map {
        in =>
          readsAll.apply(in).foldRight(Valid(HNil): ValidatedNel[Throwable, HNil])(Sequencer)
      }
      val sized = Validated.fromOption(
        opt,
        WrongArity(toInt(), in.size): Throwable
      ).toValidatedNel[Throwable, ValidatedNel[Throwable, L]]

      sized andThen {
        validatedL => validatedL map {
          l => gen.from(l)
        }
      }
    }
  }
}
