package reeds.generic

import cats.data.{Validated, ValidatedNel}
import cats.syntax.traverse._
import cats.std.option._
import cats.std.list._
import Validated._
import cats.{Functor, Traverse}
import reeds.{FReads, Reads}
import shapeless._
import shapeless.labelled._
import shapeless.ops.record.Selector

import scala.collection.GenTraversable

import scala.language.higherKinds

trait MapReadsAll[L <: HList, D <: HList] {
  type Out <: HList
  def apply(in: Map[String, String], defaults: D) : Out
}

/**
  * The priority structure is designed so that if there is a default available for a key, it will be surfaced with
  * higher priority; but if the default record does not include a key, an instance can still be summoned.
  */
object MapReadsAll extends MapReadsAll1 {

  def apply[L <: HList](implicit mapReadsAll: MapReadsAll[L, HNil]) = mapReadsAll
  def apply[L <: HList, D <: HList](implicit mapReadsAll: MapReadsAll[L, D], dummyImplicit: DummyImplicit) = mapReadsAll

  type Aux[L <: HList, D <: HList, Out0 <: HList] = MapReadsAll[L, D] { type Out = Out0 }
  type WithoutDefaults[L <: HList] = MapReadsAll[L, HNil]
  type AuxWithoutDefaults[L <: HList, Out0 <: HList] = MapReadsAll.Aux[L, HNil, Out0]

  implicit def hnil[D <: HList] : MapReadsAll.Aux[HNil, D, HNil] = new MapReadsAll[HNil, D] {
    type Out = HNil
    override def apply(
      in: Map[String, String],
      defaults: D
    ): HNil = HNil
  }

  implicit def hconsOptionWithDefault[K <: Symbol, V, D <: HList, T <: HList, TO <: HList](implicit
      headReads: Reads[V],
      witness: Witness.Aux[K],
      defaultForK: Selector.Aux[D, K, Option[V]],
      tailReadsAll: MapReadsAll.Aux[T, D, TO]) : MapReadsAll.Aux[FieldType[K, Option[V]] :: T, D, FieldType[K, ValidatedNel[Throwable, Option[V]]] :: TO] =
    new MapReadsAll[FieldType[K, Option[V]] :: T, D] {
      type Out = FieldType[K, ValidatedNel[Throwable, Option[V]]] :: TO
      def apply(in: Map[String, String], defaults: D) = {
        val key = witness.value.name
        val opt = in.get(key)
          .map(headReads.apply)
          .orElse(defaultForK.apply(defaults).map(Valid(_)))
        val head = field[K].apply(opt.sequenceU)
        val tail = tailReadsAll.apply(in, defaults)
        head :: tail
      }
    }

}

trait MapReadsAll1 extends MapReadsAll2 {

  implicit def hconsOption[K <: Symbol, V, D <: HList, T <: HList, TO <: HList](implicit
      headReads: Reads[V],
      witness: Witness.Aux[K],
      tailReadsAll: MapReadsAll.Aux[T, D, TO]) : MapReadsAll.Aux[FieldType[K, Option[V]] :: T, D, FieldType[K, ValidatedNel[Throwable, Option[V]]] :: TO] =
    new MapReadsAll[FieldType[K, Option[V]] :: T, D] {
      type Out = FieldType[K, ValidatedNel[Throwable, Option[V]]] :: TO
      def apply(in: Map[String, String], defaults: D) = {
        val key = witness.value.name
        val head = field[K].apply(in.get(key).map(headReads.apply).sequenceU)
        val tail = tailReadsAll.apply(in, defaults)
        head :: tail
      }
    }
}

trait MapReadsAll2 extends LowPriorityMapReadsAll {

  implicit def hconsNonOptionWithDefault[K <: Symbol, V, D <: HList, T <: HList, TO <: HList](implicit
      headReads: Reads[V],
      witness: Witness.Aux[K],
      defaultForK: Selector.Aux[D, K, V],
      tailReadsAll: MapReadsAll.Aux[T, D, TO]) : MapReadsAll.Aux[FieldType[K, V] :: T, D, FieldType[K, ValidatedNel[Throwable, V]] :: TO] =
    new MapReadsAll[FieldType[K, V] :: T, D] {
      type Out = FieldType[K, ValidatedNel[Throwable, V]] :: TO
      def apply(in: Map[String, String], defaults: D) = {
        val key = witness.value.name
        val value = in.get(key).map(headReads.apply).getOrElse(Valid(defaultForK.apply(defaults)))
        field[K].apply(value) :: tailReadsAll.apply(in, defaults)
      }
    }

}

trait LowPriorityMapReadsAll {


  // This is low priority so that the special case for Option[T] will take precedence, even though Option[T] has a
  // lifted instance Reads[Option[T]]
  implicit def hconsNonOption[K <: Symbol, V, D <: HList, T <: HList, TO <: HList](implicit
      headReads: Reads[V],
      witness: Witness.Aux[K],
      tailReadsAll: MapReadsAll.Aux[T, D, TO]) : MapReadsAll.Aux[FieldType[K, V] :: T, D, FieldType[K, ValidatedNel[Throwable, V]] :: TO] =
    new MapReadsAll[FieldType[K, V] :: T, D] {
      type Out = FieldType[K, ValidatedNel[Throwable, V]] :: TO
      def apply(in: Map[String, String], defaults: D) = {
        val key = witness.value.name
        val opt = in.get(key) map headReads.apply
        val headValid = Validated.fromOption(opt, MissingField(key) : Throwable).fold(
          Invalid(_).toValidatedNel,
          identity
        )
        field[K].apply(headValid) :: tailReadsAll.apply(in, defaults)
      }
    }

}

trait MapFReadsAll[F[T] <: GenTraversable[T], L <: HList, D <: HList] {
  type Out <: HList
  def apply(in: Map[String, F[String]], defaults: D) : Out
}

object MapFReadsAll extends MapFReadsAll1 {

  type Aux[F[T] <: GenTraversable[T], L <: HList, D <: HList, Out0 <: HList] = MapFReadsAll[F, L, D] {type Out = Out0}

  implicit def hnil[F[T] <: GenTraversable[T], D <: HList]: MapFReadsAll.Aux[F, HNil, D, HNil] = new MapFReadsAll[F, HNil, D] {
    type Out = HNil

    def apply(
      in: Map[String, F[String]],
      defaults: D
    ): HNil = HNil
  }

  implicit def hconsFOptionWithDefault[F[X] <: GenTraversable[X] : Traverse, K <: Symbol, V, D <: HList, T <: HList, TO <: HList](implicit
      headReads: FReads[F, V],
      witness: Witness.Aux[K],
      defaultForK: Selector.Aux[D, K, Option[F[V]]],
      tailReadsAll: MapFReadsAll.Aux[F, T, D, TO]) : MapFReadsAll.Aux[F, FieldType[K, Option[F[V]]] :: T, D, FieldType[K, ValidatedNel[Throwable, Option[F[V]]]] :: TO] =
    new MapFReadsAll[F, FieldType[K, Option[F[V]]] :: T, D] {
      type Out = FieldType[K, ValidatedNel[Throwable, Option[F[V]]]] :: TO
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val headF : ValidatedNel[Throwable, Option[F[V]]] = in.get(key).map(headReads.apply).map(_.sequenceU).sequenceU
        val default = defaultForK.apply(defaults)
        val head = headF.map(opt => opt orElse default)
        val tail = tailReadsAll.apply(in, defaults)
        field[K].apply(head) :: tail
      }
    }

  implicit def hconsOptionWithDefault[F[X] <: GenTraversable[X], K <: Symbol, V, D <: HList, T <: HList, TO <: HList](implicit
      headReads: FReads[F, V],
      witness: Witness.Aux[K],
      defaultForK: Selector.Aux[D, K, Option[V]],
      tailReadsAll: MapFReadsAll.Aux[F, T, D, TO]) : MapFReadsAll.Aux[F, FieldType[K, Option[V]] :: T, D, FieldType[K, ValidatedNel[Throwable, Option[V]]] :: TO] =
    new MapFReadsAll[F, FieldType[K, Option[V]] :: T, D] {
      type Out = FieldType[K, ValidatedNel[Throwable, Option[V]]] :: TO
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val headF : ValidatedNel[Throwable, Option[V]] = in.get(key).map(headReads.apply) match {
          case Some(f) if f.size > 1 => Invalid(WrongFieldArity(key, 1, f.size)).toValidatedNel
          case Some(f) => f.headOption.sequenceU : ValidatedNel[Throwable, Option[V]]
          case None => Valid(None) : ValidatedNel[Throwable, Option[V]]
        }
        val default = defaultForK.apply(defaults)
        val head = headF.map(opt => opt orElse default)
        val tail = tailReadsAll.apply(in, defaults)
        field[K].apply(head) :: tail
      }
    }

}

trait MapFReadsAll1 extends MapFReadsAll2 {

  implicit def hconsFOption[F[X] <: GenTraversable[X] : Traverse, K <: Symbol, V, D <: HList, T <: HList, TO <: HList](implicit
      headReads: FReads[F, V],
      witness: Witness.Aux[K],
      tailReadsAll: MapFReadsAll.Aux[F, T, D, TO]) : MapFReadsAll.Aux[F, FieldType[K, Option[F[V]]] :: T, D, FieldType[K, ValidatedNel[Throwable, Option[F[V]]]] :: TO] =
    new MapFReadsAll[F, FieldType[K, Option[F[V]]] :: T, D] {
      type Out = FieldType[K, ValidatedNel[Throwable, Option[F[V]]]] :: TO
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val headF : ValidatedNel[Throwable, Option[F[V]]] = in.get(key).map(headReads.apply).map(_.sequenceU).sequenceU
        val tail = tailReadsAll.apply(in, defaults)
        field[K].apply(headF) :: tail
      }
    }

  implicit def hconsOption[F[X] <: GenTraversable[X], K <: Symbol, V, D <: HList, T <: HList, TO <: HList](implicit
      headReads: FReads[F, V],
      witness: Witness.Aux[K],
      tailReadsAll: MapFReadsAll.Aux[F, T, D, TO]) : MapFReadsAll.Aux[F, FieldType[K, Option[V]] :: T, D, FieldType[K, ValidatedNel[Throwable, Option[V]]] :: TO] =
    new MapFReadsAll[F, FieldType[K, Option[V]] :: T, D] {
      type Out = FieldType[K, ValidatedNel[Throwable, Option[V]]] :: TO
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val headF : ValidatedNel[Throwable, Option[V]] = in.get(key).map(headReads.apply) match {
          case Some(f) if f.size > 1 => Invalid(WrongFieldArity(key, 1, f.size)).toValidatedNel
          case Some(f) => f.headOption.sequenceU : ValidatedNel[Throwable, Option[V]]
          case None => Valid(None) : ValidatedNel[Throwable, Option[V]]
        }
          //.flatMap(_.headOption).sequenceU)
        val tail = tailReadsAll.apply(in, defaults)
        field[K].apply(headF) :: tail
      }
    }


}

trait MapFReadsAll2 extends LowPriorityMapFReadsAll {

  implicit def hconsNonOptionWithDefault[F[X] <: GenTraversable[X], K <: Symbol, V, D <: HList, T <: HList, TO <: HList](implicit
      headReads: FReads[F, V],
      witness: Witness.Aux[K],
      defaultForK: Selector.Aux[D, K, V],
      tailReadsAll: MapFReadsAll.Aux[F, T, D, TO]) : MapFReadsAll.Aux[F, FieldType[K, V] :: T, D, FieldType[K, ValidatedNel[Throwable, V]] :: TO] =
    new MapFReadsAll[F, FieldType[K, V] :: T, D] {
      type Out = FieldType[K, ValidatedNel[Throwable, V]] :: TO
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val headValid = (in.get(key).map(headReads.apply) match {
          case Some(f) if f.size > 1 => Invalid(WrongFieldArity(key, 1, f.size) : Throwable).toValidatedNel
          case Some(f) => Validated.fromOption(f.headOption, WrongFieldArity(key, 1, 0) : Throwable).toValidatedNel
          case None => Valid(Valid(defaultForK.apply(defaults)))
        }).fold(Invalid(_), identity)
        field[K].apply(headValid) :: tailReadsAll.apply(in, defaults)
      }
    }

  implicit def hconsFNonOptionWithDefault[F[X] <: GenTraversable[X] : Traverse, K <: Symbol, V, D <: HList, T <: HList, TO <: HList](implicit
      headReads: FReads[F, V],
      witness: Witness.Aux[K],
      defaultForK: Selector.Aux[D, K, F[V]],
      tailReadsAll: MapFReadsAll.Aux[F, T, D, TO]) : MapFReadsAll.Aux[F, FieldType[K, F[V]] :: T, D, FieldType[K, ValidatedNel[Throwable, F[V]]] :: TO] =
    new MapFReadsAll[F, FieldType[K, F[V]] :: T, D] {
      type Out = FieldType[K, ValidatedNel[Throwable, F[V]]] :: TO
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val head = in.get(key).map(headReads.apply).map(_.sequenceU : ValidatedNel[Throwable, F[V]]).getOrElse(Valid(defaultForK.apply(defaults)))
        field[K].apply(head) :: tailReadsAll.apply(in, defaults)
      }
    }

}

trait LowPriorityMapFReadsAll {

  // This is low priority so that the special case for Option[T] will take precedence, even though Option[T] has a
  // lifted instance Reads[Option[T]]
  implicit def hconsNonOption[F[X] <: GenTraversable[X], K <: Symbol, V, D <: HList, T <: HList, TO <: HList](implicit
    headReads: FReads[F, V],
    witness: Witness.Aux[K],
    tailReadsAll: MapFReadsAll.Aux[F, T, D, TO]) : MapFReadsAll.Aux[F, FieldType[K, V] :: T, D, FieldType[K, ValidatedNel[Throwable, V]] :: TO] =
    new MapFReadsAll[F, FieldType[K, V] :: T, D] {
      type Out = FieldType[K, ValidatedNel[Throwable, V]] :: TO
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val headValid = (in.get(key).map(headReads.apply) match {
          case Some(f) if f.size > 1 => Invalid(WrongFieldArity(key, 1, f.size) : Throwable).toValidatedNel
          case Some(f) => Validated.fromOption(f.headOption, WrongFieldArity(key, 1, 0) : Throwable).toValidatedNel
          case None => Invalid(MissingField(key) : Throwable).toValidatedNel
        }).fold(Invalid(_), identity)
        field[K].apply(headValid) :: tailReadsAll.apply(in, defaults)
      }
    }

  implicit def hconsFNonOption[F[X] <: GenTraversable[X] : Traverse, K <: Symbol, V, D <: HList, T <: HList, TO <: HList](implicit
    headReads: FReads[F, V],
    witness: Witness.Aux[K],
    tailReadsAll: MapFReadsAll.Aux[F, T, D, TO]) : MapFReadsAll.Aux[F, FieldType[K, F[V]] :: T, D, FieldType[K, ValidatedNel[Throwable, F[V]]] :: TO] =
    new MapFReadsAll[F, FieldType[K, F[V]] :: T, D] {
      type Out = FieldType[K, ValidatedNel[Throwable, F[V]]] :: TO
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val head = in.get(key).map(headReads.apply)
        val headValid = Validated.fromOption(head, MissingField(key) : Throwable).fold(
          Invalid(_).toValidatedNel,
          f => f.sequenceU : ValidatedNel[Throwable, F[V]]
        )
        field[K].apply(headValid) :: tailReadsAll.apply(in, defaults)
      }
    }

}