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

trait MapReadsAll[T] {
  type Defaults <: HList
  def apply(in: Map[String, String], defaults: Defaults) : ValidatedNel[Throwable, T]
}

/**
  * The priority structure is designed so that if there is a default available for a key, it will be surfaced with
  * higher priority; but if the default record does not include a key, an instance can still be summoned.
  */
object MapReadsAll extends MapReadsAll1 {

  type Aux[T, D <: HList] = MapReadsAll[T] { type Defaults = D }

  def apply[T](implicit mapReadsAll: MapReadsAll[T]) = mapReadsAll
  def apply[T, D <: HList](implicit mapReadsAll: MapReadsAll.Aux[T, D], dummyImplicit: DummyImplicit) = mapReadsAll

  type WithoutDefaults[L <: HList] = MapReadsAll.Aux[L, HNil]

  implicit def hnil[D <: HList] : MapReadsAll.Aux[HNil, D] = new MapReadsAll[HNil] {
    type Defaults = D
    override def apply(
      in: Map[String, String],
      defaults: D
    ): ValidatedNel[Throwable, HNil] = Valid(HNil)
  }

  implicit def hconsOptionWithDefault[K <: Symbol, V, D <: HList, T <: HList](implicit
      headReads: Reads[V],
      witness: Witness.Aux[K],
      defaultForK: Selector.Aux[D, K, Option[V]],
      tailReadsAll: MapReadsAll.Aux[T, D]) : MapReadsAll.Aux[FieldType[K, Option[V]] :: T, D] =
    new MapReadsAll[FieldType[K, Option[V]] :: T] {
      type Defaults = D
      def apply(in: Map[String, String], defaults: D) = {
        val key = witness.value.name
        val opt = in.get(key)
          .map(headReads.apply)
          .orElse(defaultForK.apply(defaults).map(Valid(_)))
        val headValid: ValidatedNel[Throwable, Option[V]] = opt.sequenceU
        headValid.map(field[K].apply).product(tailReadsAll.apply(in, defaults)).map(prependProduct)
      }
    }

}

trait MapReadsAll1 extends MapReadsAll2 {

  implicit def caseClass[T, L <: HList, D <: HList](implicit
    gen: LabelledGeneric.Aux[T, L],
    default: Default.AsRecord.Aux[T, D],
    mapReadsAll: MapReadsAll.Aux[L, D]
  ) : MapReadsAll.Aux[T, D] = new MapReadsAll[T] {
    type Defaults = D
    def apply(in: Map[String, String], defaults: D) = mapReadsAll.apply(in, defaults).map {
      l => gen.from(l)
    }
  }

  implicit def hconsOption[K <: Symbol, V, D <: HList, T <: HList](implicit
      headReads: Reads[V],
      witness: Witness.Aux[K],
      tailReadsAll: MapReadsAll.Aux[T, D]) : MapReadsAll.Aux[FieldType[K, Option[V]] :: T, D] =
    new MapReadsAll[FieldType[K, Option[V]] :: T] {
      type Defaults = D
      def apply(in: Map[String, String], defaults: D) = {
        val key = witness.value.name
        val headValid: ValidatedNel[Throwable, Option[V]] = in.get(key).map(headReads.apply).sequenceU
        headValid.map(field[K].apply).product(tailReadsAll.apply(in, defaults)).map(prependProduct)
      }
    }
}

trait MapReadsAll2 extends LowPriorityMapReadsAll {

  implicit def hconsNonOptionWithDefault[K <: Symbol, V, D <: HList, T <: HList](implicit
      headReads: Reads[V],
      witness: Witness.Aux[K],
      defaultForK: Selector.Aux[D, K, V],
      tailReadsAll: MapReadsAll.Aux[T, D]) : MapReadsAll.Aux[FieldType[K, V] :: T, D] =
    new MapReadsAll[FieldType[K, V] :: T] {
      type Defaults = D
      def apply(in: Map[String, String], defaults: D) = {
        val key = witness.value.name
        val headValid = in.get(key).map(headReads.apply).getOrElse(Valid(defaultForK.apply(defaults)))
        headValid.map(field[K].apply).product(tailReadsAll.apply(in, defaults)).map(prependProduct)
      }
    }

}

trait LowPriorityMapReadsAll {

  private[generic] def prependProduct[A, B <: HList](p: (A, B)): A :: B = p._1 :: p._2


  // This is low priority so that the special case for Option[T] will take precedence, even though Option[T] has a
  // lifted instance Reads[Option[T]]
  implicit def hconsNonOption[K <: Symbol, V, D <: HList, T <: HList](implicit
      headReads: Reads[V],
      witness: Witness.Aux[K],
      tailReadsAll: MapReadsAll.Aux[T, D]) : MapReadsAll.Aux[FieldType[K, V] :: T, D] =
    new MapReadsAll[FieldType[K, V] :: T] {
      type Defaults = D
      def apply(in: Map[String, String], defaults: D) = {
        val key = witness.value.name
        val opt = in.get(key) map headReads.apply
        val headValid = Validated.fromOption(opt, MissingField(key) : Throwable).fold(
          Invalid(_).toValidatedNel,
          identity
        )
        headValid.map(field[K].apply).product(tailReadsAll.apply(in, defaults)).map(prependProduct)
      }
    }

}

trait MapFReadsAll[F[T] <: GenTraversable[T], T] {
  type Defaults <: HList
  def apply(in: Map[String, F[String]], defaults: Defaults) : ValidatedNel[Throwable, T]
}

object MapFReadsAll extends MapFReadsAll1 {

  type Aux[F[T] <: GenTraversable[T], T, D <: HList] = MapFReadsAll[F, T] { type Defaults = D }

  def apply[F[T] <: GenTraversable[T], T](implicit reads: MapFReadsAll[F, T]) = reads

  implicit def caseClass[F[T] <: GenTraversable[T], T, L <: HList, D <: HList](implicit
    gen: LabelledGeneric.Aux[T, L],
    default: Default.AsRecord.Aux[T, D],
    mapFReadsAll: MapFReadsAll.Aux[F, L, D]
  ) : MapFReadsAll.Aux[F, T, D] = new MapFReadsAll[F, T] {
    type Defaults = D
    def apply(in: Map[String, F[String]], defaults: D) = mapFReadsAll.apply(in, defaults).map {
      l => gen.from(l)
    }
  }

  implicit def hnil[F[T] <: GenTraversable[T], D <: HList]: MapFReadsAll.Aux[F, HNil, D] = new MapFReadsAll[F, HNil] {
    type Defaults = D
    def apply(
      in: Map[String, F[String]],
      defaults: D
    ) = Valid(HNil)
  }

  implicit def hconsFOptionWithDefault[F[X] <: GenTraversable[X] : Traverse, K <: Symbol, V, D <: HList, T <: HList](implicit
      headReads: FReads[F, V],
      witness: Witness.Aux[K],
      defaultForK: Selector.Aux[D, K, Option[F[V]]],
      tailReadsAll: MapFReadsAll.Aux[F, T, D]) : MapFReadsAll.Aux[F, FieldType[K, Option[F[V]]] :: T, D] =
    new MapFReadsAll[F, FieldType[K, Option[F[V]]] :: T] {
      type Defaults = D
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val headF : ValidatedNel[Throwable, Option[F[V]]] = in.get(key).map(headReads.apply).map(_.sequenceU).sequenceU
        val default = defaultForK.apply(defaults)
        val head = headF.map(opt => opt orElse default)
        val tail = tailReadsAll.apply(in, defaults)
        head.map(field[K].apply).product(tail).map(prependProduct)
      }
    }

  implicit def hconsOptionWithDefault[F[X] <: GenTraversable[X], K <: Symbol, V, D <: HList, T <: HList](implicit
      headReads: FReads[F, V],
      witness: Witness.Aux[K],
      defaultForK: Selector.Aux[D, K, Option[V]],
      tailReadsAll: MapFReadsAll.Aux[F, T, D]) : MapFReadsAll.Aux[F, FieldType[K, Option[V]] :: T, D] =
    new MapFReadsAll[F, FieldType[K, Option[V]] :: T] {
      type Defaults = D
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val headF : ValidatedNel[Throwable, Option[V]] = in.get(key).map(headReads.apply) match {
          case Some(f) if f.size > 1 => Invalid(WrongFieldArity(key, 1, f.size)).toValidatedNel
          case Some(f) => f.headOption.sequenceU : ValidatedNel[Throwable, Option[V]]
          case None => Valid(None) : ValidatedNel[Throwable, Option[V]]
        }
        val head = headF.map(opt => opt orElse defaultForK.apply(defaults))
        val tail = tailReadsAll.apply(in, defaults)
        head.map(field[K].apply).product(tail).map(prependProduct)
      }
    }

}

trait MapFReadsAll1 extends MapFReadsAll2 {

  implicit def hconsFOption[F[X] <: GenTraversable[X] : Traverse, K <: Symbol, V, D <: HList, T <: HList](implicit
      headReads: FReads[F, V],
      witness: Witness.Aux[K],
      tailReadsAll: MapFReadsAll.Aux[F, T, D]) : MapFReadsAll.Aux[F, FieldType[K, Option[F[V]]] :: T, D] =
    new MapFReadsAll[F, FieldType[K, Option[F[V]]] :: T] {
      type Defaults = D
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val headF : ValidatedNel[Throwable, Option[F[V]]] = in.get(key).map(headReads.apply).map(_.sequenceU).sequenceU
        val tail = tailReadsAll.apply(in, defaults)
        headF.map(field[K].apply).product(tail).map(prependProduct)
      }
    }

  implicit def hconsOption[F[X] <: GenTraversable[X], K <: Symbol, V, D <: HList, T <: HList](implicit
      headReads: FReads[F, V],
      witness: Witness.Aux[K],
      tailReadsAll: MapFReadsAll.Aux[F, T, D]) : MapFReadsAll.Aux[F, FieldType[K, Option[V]] :: T, D] =
    new MapFReadsAll[F, FieldType[K, Option[V]] :: T] {
      type Defaults = D
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val headF : ValidatedNel[Throwable, Option[V]] = in.get(key).map(headReads.apply) match {
          case Some(f) if f.size > 1 => Invalid(WrongFieldArity(key, 1, f.size)).toValidatedNel
          case Some(f) => f.headOption.sequenceU : ValidatedNel[Throwable, Option[V]]
          case None => Valid(None) : ValidatedNel[Throwable, Option[V]]
        }
        val tail = tailReadsAll.apply(in, defaults)
        headF.map(field[K].apply).product(tail).map(prependProduct)
      }
    }


}

trait MapFReadsAll2 extends LowPriorityMapFReadsAll {

  implicit def hconsNonOptionWithDefault[F[X] <: GenTraversable[X], K <: Symbol, V, D <: HList, T <: HList](implicit
      headReads: FReads[F, V],
      witness: Witness.Aux[K],
      defaultForK: Selector.Aux[D, K, V],
      tailReadsAll: MapFReadsAll.Aux[F, T, D]) : MapFReadsAll.Aux[F, FieldType[K, V] :: T, D] =
    new MapFReadsAll[F, FieldType[K, V] :: T] {
      type Defaults = D
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val headValid = (in.get(key).map(headReads.apply) match {
          case Some(f) if f.size > 1 => Invalid(WrongFieldArity(key, 1, f.size) : Throwable).toValidatedNel
          case Some(f) => Validated.fromOption(f.headOption, WrongFieldArity(key, 1, 0) : Throwable).toValidatedNel
          case None => Valid(Valid(defaultForK.apply(defaults)))
        }).fold(Invalid(_), identity)
        headValid.map(field[K].apply).product(tailReadsAll.apply(in, defaults)).map(prependProduct)
      }
    }

  implicit def hconsFNonOptionWithDefault[F[X] <: GenTraversable[X] : Traverse, K <: Symbol, V, D <: HList, T <: HList](implicit
      headReads: FReads[F, V],
      witness: Witness.Aux[K],
      defaultForK: Selector.Aux[D, K, F[V]],
      tailReadsAll: MapFReadsAll.Aux[F, T, D]) : MapFReadsAll.Aux[F, FieldType[K, F[V]] :: T, D] =
    new MapFReadsAll[F, FieldType[K, F[V]] :: T] {
      type Defaults = D
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val head = in.get(key).map(headReads.apply)
          .map(_.sequenceU : ValidatedNel[Throwable, F[V]])
          .getOrElse(Valid(defaultForK.apply(defaults)))

        head.map(field[K].apply).product(tailReadsAll.apply(in, defaults)).map(prependProduct)
      }
    }

}

trait LowPriorityMapFReadsAll {

  private[generic] def prependProduct[A, B <: HList](p: (A, B)): A :: B = p._1 :: p._2

  // This is low priority so that the special case for Option[T] will take precedence, even though Option[T] has a
  // lifted instance Reads[Option[T]]
  implicit def hconsNonOption[F[X] <: GenTraversable[X], K <: Symbol, V, D <: HList, T <: HList](implicit
    headReads: FReads[F, V],
    witness: Witness.Aux[K],
    tailReadsAll: MapFReadsAll.Aux[F, T, D]) : MapFReadsAll.Aux[F, FieldType[K, V] :: T, D] =
    new MapFReadsAll[F, FieldType[K, V] :: T] {
      type Defaults = D
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val headValid = (in.get(key).map(headReads.apply) match {
          case Some(f) if f.size > 1 => Invalid(WrongFieldArity(key, 1, f.size) : Throwable).toValidatedNel
          case Some(f) => Validated.fromOption(f.headOption, WrongFieldArity(key, 1, 0) : Throwable).toValidatedNel
          case None => Invalid(MissingField(key) : Throwable).toValidatedNel
        }).fold(Invalid(_), identity)
        headValid.map(field[K].apply).product(tailReadsAll.apply(in, defaults)).map(prependProduct)
      }
    }

  implicit def hconsFNonOption[F[X] <: GenTraversable[X] : Traverse, K <: Symbol, V, D <: HList, T <: HList](implicit
    headReads: FReads[F, V],
    witness: Witness.Aux[K],
    tailReadsAll: MapFReadsAll.Aux[F, T, D]) : MapFReadsAll.Aux[F, FieldType[K, F[V]] :: T, D] =
    new MapFReadsAll[F, FieldType[K, F[V]] :: T] {
      type Defaults = D
      def apply(in: Map[String, F[String]], defaults: D) = {
        val key = witness.value.name
        val head = in.get(key).map(headReads.apply)
        val headValid = Validated.fromOption(head, MissingField(key) : Throwable).fold(
          Invalid(_).toValidatedNel,
          f => f.sequenceU : ValidatedNel[Throwable, F[V]]
        )
        headValid.map(field[K].apply).product(tailReadsAll.apply(in, defaults)).map(prependProduct)
      }
    }

}