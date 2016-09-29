package reeds.generic

import cats.Traverse
import cats.data.{Validated, ValidatedNel}
import cats.syntax.traverse._
import cats.instances.list._
import reeds.{FReads, Reads}
import shapeless._
import shapeless.ops.hlist.IsHCons

trait ReadsAll[L <: HList] {
  type Out <: HList
  type In <: HList
  def apply(in: In) : Out
}

object ReadsAll {

  type AuxIn[L <: HList, In0 <: HList] = ReadsAll[L] { type In = In0 }
  type AuxInOut[L <: HList, In0 <: HList, Out0 <: HList] = ReadsAll[L] { type In = In0; type Out = Out0 }

  implicit def hconsFunctorReadsAll[F[_] : Traverse, H, T <: HList, TO <: HList, In0 <: HList, IT <: HList, Out0 <: HList](implicit
    headFReads: FReads[F, H],
    tailReadsAll: ReadsAll.AuxInOut[T, IT, TO],
    isHCons: IsHCons.Aux[In0, F[String], IT],
    outHCons: IsHCons.Aux[Out0, ValidatedNel[Throwable, F[H]], TO]
  ) : ReadsAll.AuxInOut[F[H] :: T, In0, ValidatedNel[Throwable, F[H]] :: TO] = new ReadsAll[F[H] :: T] {
    type Out = ValidatedNel[Throwable, F[H]] :: tailReadsAll.Out
    type In = In0
    def apply(input: In0) : Out =
      headFReads.apply(isHCons.head(input)).sequenceU :: tailReadsAll.apply(isHCons.tail(input))
  }

  implicit def hconsScalarReadsAll[H, T <: HList, In0 <: HList, IT <: HList, Out0 <: HList](implicit
    headReads: Reads[H],
    tailReadsAll: ReadsAll.AuxIn[T, IT],
    isHCons: IsHCons.Aux[In0, String, IT]
  ) : ReadsAll.AuxInOut[H :: T, In0, ValidatedNel[Throwable, H] :: tailReadsAll.Out] = new ReadsAll[H :: T] {
    type Out = ValidatedNel[Throwable, H] :: tailReadsAll.Out
    type In = In0
    def apply(input: In0) : Out =
      headReads.apply(isHCons.head(input)) :: tailReadsAll.apply(isHCons.tail(input))
  }

  implicit val hnilReadsAll : ReadsAll.AuxInOut[HNil, HNil, HNil] = new ReadsAll[HNil] {
    type Out = HNil
    type In = HNil
    def apply(in: HNil) = HNil
  }

}
