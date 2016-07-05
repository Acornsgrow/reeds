package reeds.generic

import cats.Functor
import cats.data.Validated.Valid
import cats.data._
import shapeless.ops.hlist.RightFolder
import shapeless.ops.record.{Keys, Values}
import shapeless.{Default, HList, HNil, LabelledGeneric}

import scala.collection.GenTraversable

import scala.language.higherKinds

trait GenericMapReads[T] {
  def apply(in: Map[String, String]) : ValidatedNel[Throwable, T]
}

object GenericMapReads {

  implicit def derive[T, L <: HList, Defaults <: HList, K <: HList, V <: HList, Validated <: HList, ValidatedRecord <: HList](implicit
      gen: LabelledGeneric.Aux[T, L],
      default: Default.AsRecord.Aux[T, Defaults],
      keys: Keys.Aux[L, K],
      values: Values.Aux[L, V],
      mapReadsAll: MapReadsAll.Aux[L, Defaults, ValidatedRecord],
      validatedValues: Values.Aux[ValidatedRecord, Validated],
      folder: RightFolder.Aux[ValidatedRecord, ValidatedNel[Throwable, HNil], FieldSequencer.type, ValidatedNel[Throwable, L]]) : GenericMapReads[T] =
    new GenericMapReads[T] {
      def apply(input: Map[String, String]) = {
        val validated = mapReadsAll.apply(input, default.apply())
        val values = validatedValues.apply(validated)
        val folded = folder.apply(validated, Valid(HNil))
        folded map {
          l => gen.from(l)
        }
      }
    }

}

trait GenericFMapReads[F[A] <: GenTraversable[A], T] {
  def apply(in: Map[String, F[String]]) : ValidatedNel[Throwable, T]
}

object GenericFMapReads {

  implicit def derive[F[A] <: GenTraversable[A], T, L <: HList, Defaults <: HList, K <: HList, V <: HList, Validated <: HList, ValidatedRecord <: HList](implicit
    gen: LabelledGeneric.Aux[T, L],
    default: Default.AsRecord.Aux[T, Defaults],
    functor: Functor[F],
    keys: Keys.Aux[L, K],
    values: Values.Aux[L, V],
    mapReadsAll: MapFReadsAll.Aux[F, L, Defaults, ValidatedRecord],
    validatedValues: Values.Aux[ValidatedRecord, Validated],
    folder: RightFolder.Aux[ValidatedRecord, ValidatedNel[Throwable, HNil], FieldSequencer.type, ValidatedNel[Throwable, L]]) : GenericFMapReads[F, T] =
  new GenericFMapReads[F, T] {
    def apply(input: Map[String, F[String]]) = {
      val validated = mapReadsAll.apply(input, default.apply())
      val values = validatedValues.apply(validated)
      val folded = folder.apply(validated, Valid(HNil))
      folded map {
        l => gen.from(l)
      }
    }
  }

}
