package reeds.generic

import java.time.LocalDate
import java.util.UUID

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import Validated._
import org.scalatest.{FeatureSpec, GivenWhenThen}
import reeds.Reads
import shapeless._
import shapeless.ops.hlist.RightFolder
import cats.std.list._
import shapeless.labelled.FieldType
import shapeless.record._

import scala.reflect.ClassTag

case class Sample(uuid: UUID, date: LocalDate, id: Int)
case class SampleWithOptional(uuid: UUID, date: Option[LocalDate], id: Int)
case class SampleWithFunctor(uuid: UUID, dates: List[LocalDate], id: Int)
case class SampleWithOptionalFunctor(uuid: UUID, dates: Option[List[LocalDate]], id: Int)
case class SampleWithOptionalAndFunctor(uuid: UUID, dates: List[LocalDate], id: Option[Int])

class FunctorTests extends FeatureSpec with GivenWhenThen {

  val validUuid = "deadbeef-dead-dead-beef-deaddeadbeef"
  val validLocalDate = "2016-03-28"
  val validInt = "10"

  val validSample = Sample(
    UUID.fromString(validUuid),
    LocalDate.parse(validLocalDate),
    validInt.toInt
  )

  feature("Generic from string functor") {

    scenario("List of valid strings") {

      Given("a list of valid strings")
      val validList = List(validUuid, validLocalDate, validInt)

      When("list is read to ADT")
      val result = validList.read[Sample]

      Then("Result should be valid")
      assert(result == Valid(validSample))

    }

    scenario("List with too few elements") {

      Given("a list with too few elements")
      val invalidList = List(validUuid)

      When("list is read to ADT")
      val result = invalidList.read[Sample]

      Then("result should be invalid")
      assert(result == Invalid(
        NonEmptyList(
          WrongArity(3, 1))))
    }

    scenario("List with too many elements") {
      Given("a list with too many elements")
      val invalidList = List(validUuid, validLocalDate, validInt, validUuid)

      When("list is read to ADT")
      val result = invalidList.read[Sample]

      Then("result should be invalid")
      assert(result == Invalid(
        NonEmptyList(
          WrongArity(3, 4))))
    }

    scenario("List with invalid elements") {
      Given("a list with invalid elements")
      val invalidList = List("not a valid UUID", validLocalDate, "not a valid int")

      When("list is read to ADT")
      val result = invalidList.read[Sample]

      Then("result should be invalid")
      assert(result.isInvalid)

      And("errors should be aggregated")
      val errors = result.toXor.swap.toOption.get.unwrap
      assert(errors.length == 2)
      assert(errors exists (err => err.getMessage == "'not a valid UUID' is not a valid UUID."))
      assert(errors exists (_.isInstanceOf[java.lang.NumberFormatException]))
    }

  }

  feature("Generic from string map") {

    scenario("map with all valid values") {
      Given("a map with all valid values")
      val validMap = Map("uuid" -> validUuid, "date" -> validLocalDate, "id" -> validInt)

      When("map is read to ADT")
      val result = validMap.readMap[Sample]

      Then("result should be valid")
      assert(result == Valid(validSample))
    }

    scenario("map with missing values") {
      Given("a map with missing values")
      val invalidMap = Map("uuid" -> validUuid)

      When("map is read to ADT")
      val result = invalidMap.readMap[Sample]

      Then("result should be invalid")
      assert(result.isInvalid)

      And("errors should be aggregated")
      val errors = result.toXor.swap.toOption.get.unwrap
      assert(errors.length == 2)
      assert(errors exists (_.getMessage == "Missing non-optional field 'date'"))
      assert(errors exists (_.getMessage == "Missing non-optional field 'id'"))
    }

    scenario("map with invalid values") {
      Given("a map with invalid values")
      val invalidMap = Map("uuid" -> "not a valid uuid", "date" -> "not a valid date", "id" -> "not a valid int")

      When("map is read to ADT")
      val result = invalidMap.readMap[Sample]

      Then("result should be invalid")
      assert(result.isInvalid)

      And("errors should be aggregated")
      val errors = result.toXor.swap.toOption.get.unwrap
      assert(errors.length == 3)
    }

  }

  feature("Generic with optional fields from string map") {

    val validSampleWithNone = SampleWithOptional(UUID.fromString(validUuid), None, validInt.toInt)
    val validSampleWithSome = SampleWithOptional(UUID.fromString(validUuid), Some(LocalDate.parse(validLocalDate)), validInt.toInt)

    scenario("map with missing optional field") {
      Given("a map with missing optional field")
      val validMap = Map("uuid" -> validUuid, "id" -> validInt)

      When("map is read to ADT")
      val result = validMap.readMap[SampleWithOptional]

      Then("result should be valid")
      assert(result == Valid(validSampleWithNone))
    }

    scenario("map with present optional field") {
      Given("a map with present optional field")
      val validMap = Map("uuid" -> validUuid, "date" -> validLocalDate, "id" -> validInt)

      When("map is read to ADT")
      val result = validMap.readMap[SampleWithOptional]

      Then("result should be valid")
      assert(result == Valid(validSampleWithSome))
    }

  }

  feature("Generic with functor fields from string functor map") {

    val validSampleWithFunctor = SampleWithFunctor(
      UUID.fromString(validUuid),
      List(LocalDate.parse(validLocalDate), LocalDate.parse(validLocalDate)),
      validInt.toInt)

    val validSampleWithNoneFunctor = SampleWithOptionalFunctor(
      UUID.fromString(validUuid),
      None,
      validInt.toInt
    )

    val validSampleWithNone = SampleWithOptional(UUID.fromString(validUuid), None, validInt.toInt)

    val validSampleWithSomeEmpty = validSampleWithNoneFunctor.copy(dates = Some(List()))

    val validSampleWithSome = SampleWithOptionalAndFunctor(
      UUID.fromString(validUuid),
      List(LocalDate.parse(validLocalDate), LocalDate.parse(validLocalDate)),
      Some(validInt.toInt)
    )

    scenario("hlist with correct values") {
      Given("an hlist with correct values in correct order")
      val validList = validUuid :: List(validLocalDate, validLocalDate) :: validInt :: HNil

      When("list is read to hlist of ADT")
      val gen = Generic[SampleWithFunctor]
      val reads = implicitly[ReadsAll[gen.Repr]]
      val result = reads.apply(validList.asInstanceOf[reads.In])

      Then("result should be valid")
      val e = validSampleWithFunctor
      val expected = Valid(e.uuid) :: Valid(e.dates) :: Valid(e.id) :: HNil
      assert(result == expected)
    }

    scenario("list map with too many values for optional field") {
      Given("a list map with too many values for optional field")
      val validMap = Map("uuid" -> List(validUuid), "date" -> List(validLocalDate, validLocalDate), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[SampleWithOptional]

      Then("result should be invalid")
      assert(result.isInvalid)
    }

    scenario("list map with empty values for optional field") {
      Given("a list map with empty values for optional field")
      val validMap = Map("uuid" -> List(validUuid), "date" -> List(), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[SampleWithOptional]

      Then("result should be valid")
      assert(result == Valid(validSampleWithNone))
    }

    scenario("list map with missing values for optional field") {
      Given("a list map with empty values for optional field")
      val validMap = Map("uuid" -> List(validUuid), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[SampleWithOptional]

      Then("result should be valid")
      assert(result == Valid(validSampleWithNone))
    }

    scenario("list map with empty values for non-optional field") {
      Given("a list map with empty values for non-optional field")
      val validMap = Map("uuid" -> List(validUuid), "date" -> List(), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[Sample]

      Then("result should be invalid")
      assert(result.isInvalid)
    }

    scenario("list map with missing values for non-optional field") {
      Given("a list map with missing values for non-optional field")
      val validMap = Map("uuid" -> List(validUuid), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[Sample]

      Then("result should be invalid")
      assert(result.isInvalid)
    }

    scenario("list map with missing values for non-optional functor field") {
      Given("a list map with missing values for non-optional field")
      val validMap = Map("uuid" -> List(validUuid), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[SampleWithFunctor]

      Then("result should be invalid")
      assert(result.isInvalid)
    }

    scenario("list map with correct number of values") {
      Given("a list map with correct number of values")
      val validMap = Map("uuid" -> List(validUuid), "dates" -> List(validLocalDate, validLocalDate), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[SampleWithFunctor]

      Then("result should be valid")
      assert(result == Valid(validSampleWithFunctor))
    }

    scenario("list map with too many values") {
      Given("a list map with too many values in a field")
      val invalidMap = Map("uuid" -> List(validUuid, validUuid), "dates" -> List(validLocalDate, validLocalDate), "id" -> List(validInt))

      When("map is read to ADT")
      val result = invalidMap.readMap[SampleWithFunctor]

      Then("result should be invalid")
      assert(result == Invalid(NonEmptyList(WrongFieldArity("uuid", 1, 2))))
    }

    scenario("list map with empty values") {
      Given("a list map with empty values")
      val invalidMap = Map("uuid" -> List(), "dates" -> List(validLocalDate, validLocalDate), "id" -> List(validInt))

      When("map is read to ADT")
      val result = invalidMap.readMap[SampleWithFunctor]

      Then("result should be invalid")
      assert(result == Invalid(NonEmptyList(WrongFieldArity("uuid", 1, 0))))
    }

    scenario("list map with optional functor") {
      Given("a list map with optional functor key missing")
      val validMap = Map("uuid" -> List(validUuid), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[SampleWithOptionalFunctor]

      Then("result should be valid")
      assert(result == Valid(validSampleWithNoneFunctor))
    }

    scenario("list map with optional functor empty") {
      Given("a list map with optional functor key empty")
      val validMap = Map("uuid" -> List(validUuid), "dates" -> List(), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[SampleWithOptionalFunctor]

      Then("result should be valid, with Some(List())")
      assert(result == Valid(validSampleWithSomeEmpty))
    }

    scenario("list map with separate optional present and functor") {
      Given("a list map with separate optional present and functor")
      val validMap = Map("uuid" -> List(validUuid), "dates" -> List(validLocalDate, validLocalDate), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[SampleWithOptionalAndFunctor]

      Then("result should be valid")
      assert(result == Valid(validSampleWithSome))
    }

  }


}
