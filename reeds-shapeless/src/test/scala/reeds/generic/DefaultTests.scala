package reeds.generic

import java.time.LocalDate
import java.util.UUID

import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.std.list._
import org.scalatest.{FeatureSpec, GivenWhenThen}

object Defaults {
  val uuid = UUID.fromString("beefdead-beef-beef-dead-beefbeefdead")
  val date = LocalDate.parse("2017-03-28")
  val id = 47
}

case class DefaultSample(
  uuid: UUID = Defaults.uuid,
  date: LocalDate = Defaults.date,
  id: Int)

case class DefaultSampleWithOptional(
  uuid: UUID = Defaults.uuid,
  date: Option[LocalDate] = Some(Defaults.date),
  id: Int)

case class DefaultSampleWithFunctor(
  uuid: UUID = Defaults.uuid,
  dates: List[LocalDate] = List(Defaults.date, Defaults.date),
  id: Int)

case class DefaultSampleWithOptionalFunctor(
  uuid: UUID,
  dates: Option[List[LocalDate]] = Some(List(Defaults.date, Defaults.date)),
  id: Int)

case class DefaultSampleWithOptionalAndFunctor(
  uuid: UUID,
  dates: List[LocalDate] = List(Defaults.date, Defaults.date),
  id: Option[Int] = Some(Defaults.id))

class DefaultTests extends FeatureSpec with GivenWhenThen {

  val validUuid = "deadbeef-dead-dead-beef-deaddeadbeef"
  val validLocalDate = "2016-03-28"
  val validInt = "10"

  val validSampleDefaultUuid = DefaultSample(
    Defaults.uuid,
    LocalDate.parse(validLocalDate),
    validInt.toInt
  )


  feature("Generic with defaults from string map with a missing default field") {

    scenario("map with all valid values") {
      Given("a map with all valid values and a missing default field")
      val validMap = Map("date" -> validLocalDate, "id" -> validInt)

      When("map is read to ADT")
      val result = validMap.readMap[DefaultSample]

      Then("result should be valid")
      assert(result == Valid(validSampleDefaultUuid))
    }

    scenario("map with missing values without default") {
      Given("a map with missing values")
      val invalidMap = Map("uuid" -> validUuid)

      When("map is read to ADT")
      val result = invalidMap.readMap[DefaultSample]

      Then("result should be invalid")
      assert(result.isInvalid)

      And("only the field without a default should be reported as an error")
      val errors = result.toXor.swap.toOption.get.unwrap
      assert(errors == List(MissingField("id")))
    }

    scenario("map with invalid values, including for default fields") {
      Given("a map with invalid values for default fields")
      val invalidMap = Map("uuid" -> "not a valid uuid", "date" -> "not a valid date", "id" -> "not a valid int")

      When("map is read to ADT")
      val result = invalidMap.readMap[DefaultSample]

      Then("result should be invalid")
      assert(result.isInvalid)

      And("errors should be aggregated")
      val errors = result.toXor.swap.toOption.get.unwrap
      assert(errors.length == 3)
    }

  }

  feature("Generic with optional fields with default Some from string map") {

    val validSampleWithDefaultSome = DefaultSampleWithOptional(UUID.fromString(validUuid), Some(Defaults.date), validInt.toInt)
    val validSampleWithSome = DefaultSampleWithOptional(UUID.fromString(validUuid), Some(LocalDate.parse(validLocalDate)), validInt.toInt)

    scenario("map with missing optional field with a default") {
      Given("a map with missing optional field with a default")
      val validMap = Map("uuid" -> validUuid, "id" -> validInt)

      When("map is read to ADT")
      val result = validMap.readMap[DefaultSampleWithOptional]

      Then("result should be valid")
      assert(result.isValid)

      And("field should contain default value")
      assert(result == Valid(validSampleWithDefaultSome))

    }

    scenario("map with present optional field") {
      Given("a map with present optional field")
      val validMap = Map("uuid" -> validUuid, "date" -> validLocalDate, "id" -> validInt)

      When("map is read to ADT")
      val result = validMap.readMap[DefaultSampleWithOptional]

      Then("result should be valid")
      assert(result.isValid)

      And("field should contain given value")
      assert(result == Valid(validSampleWithSome))
    }

    scenario("list map with missing value for an optional field with default Some") {
      Given("a list map with missing values in a field with a default value")
      val invalidMap = Map("uuid" -> List(validUuid), "id" -> List(validInt))

      When("map is read to ADT")
      val result = invalidMap.readMap[DefaultSampleWithOptional]

      Then("result should be valid")
      assert(result == Valid(validSampleWithDefaultSome))
    }

    scenario("list map with missing value for an non-optional field with default") {
      Given("a list map with empty values in a field with a default value")
      val invalidMap = Map("date" -> List(validLocalDate), "id" -> List(validInt))

      When("map is read to ADT")
      val result = invalidMap.readMap[DefaultSample]

      Then("result should be valid")
      assert(result == Valid(validSampleDefaultUuid))
    }

    scenario("list map with provided value for an optional field with default Some") {
      Given("a list map with nonempty values in a field with a default value")
      val invalidMap = Map("uuid" -> List(validUuid), "date" -> List(validLocalDate), "id" -> List(validInt))

      When("map is read to ADT")
      val result = invalidMap.readMap[DefaultSampleWithOptional]

      Then("result should be valid")
      assert(result == Valid(validSampleWithSome))
    }

    scenario("list map with provided empty list for an optional field with default Some") {
      Given("a list map with empty values in a field with a default value")
      val invalidMap = Map("uuid" -> List(validUuid), "date" -> List(), "id" -> List(validInt))

      When("map is read to ADT")
      val result = invalidMap.readMap[DefaultSampleWithOptional]

      Then("result should be valid")
      assert(result == Valid(validSampleWithDefaultSome))
    }

    scenario("list map with too many provided values for an optional field with default Some") {
      Given("a list map with too many values in a field with a default value")
      val invalidMap = Map("uuid" -> List(validUuid), "date" -> List(validUuid, validUuid), "id" -> List(validInt))

      When("map is read to ADT")
      val result = invalidMap.readMap[DefaultSampleWithOptional]

      Then("result should be invalid")
      assert(result.isInvalid)
    }

  }

  feature("Generic with functor fields and default values from string functor map") {

    val validSampleWithDefaultFunctor = DefaultSampleWithFunctor(
      UUID.fromString(validUuid),
      List(Defaults.date, Defaults.date),
      validInt.toInt)

    val validSampleWithDefaultSomeFunctor = DefaultSampleWithOptionalFunctor(
      UUID.fromString(validUuid),
      Some(List(Defaults.date, Defaults.date)),
      validInt.toInt
    )

    val validSampleWithSomeEmpty = validSampleWithDefaultSomeFunctor.copy(dates = Some(List()))

    val validSampleWithSome = DefaultSampleWithOptionalAndFunctor(
      UUID.fromString(validUuid),
      List(LocalDate.parse(validLocalDate), LocalDate.parse(validLocalDate)),
      Some(validInt.toInt)
    )

    scenario("list map with missing default list field") {
      Given("a list map with correct number of values")
      val validMap = Map("uuid" -> List(validUuid), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[DefaultSampleWithFunctor]

      Then("result should be valid")
      assert(result == Valid(validSampleWithDefaultFunctor))
    }

    scenario("list map with too many values for a field with default") {
      Given("a list map with too many values in a field with a default value")
      val invalidMap = Map("uuid" -> List(validUuid, validUuid), "dates" -> List(validLocalDate, validLocalDate), "id" -> List(validInt))

      When("map is read to ADT")
      val result = invalidMap.readMap[DefaultSampleWithFunctor]

      Then("result should be invalid")
      assert(result == Invalid(NonEmptyList(WrongFieldArity("uuid", 1, 2))))
    }

    scenario("list map with empty values for a field with default") {
      Given("a list map with empty values in a field with a default value")
      val invalidMap = Map("uuid" -> List(), "dates" -> List(validLocalDate, validLocalDate), "id" -> List(validInt))

      When("map is read to ADT")
      val result = invalidMap.readMap[DefaultSampleWithFunctor]

      Then("result should be invalid")
      assert(result == Invalid(NonEmptyList(WrongFieldArity("uuid", 1, 0))))
    }

    scenario("list map with optional functor with default Some") {
      Given("a list map with optional functor key missing and a default Some")
      val validMap = Map("uuid" -> List(validUuid), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[DefaultSampleWithOptionalFunctor]

      Then("result should be valid")
      assert(result.isValid)

      And("result should contain default Some functor value")
      assert(result == Valid(validSampleWithDefaultSomeFunctor))
    }

    scenario("list map with optional functor present but empty, and a default non-empty Some") {
      Given("a list map with optional functor key present but empty, and a default non-empty Some")
      val validMap = Map("uuid" -> List(validUuid), "dates" -> List(), "id" -> List(validInt))

      When("map is read to ADT")
      val result = validMap.readMap[DefaultSampleWithOptionalFunctor]

      Then("result should be valid, with Some(List())")
      assert(result == Valid(validSampleWithSomeEmpty))
    }

  }


}
