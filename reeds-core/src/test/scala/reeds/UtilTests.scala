package reeds

import java.util.{Currency, UUID}

import cats.data.Validated.Valid
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FreeSpec
import org.scalatest.prop.{PropertyChecks, TableDrivenPropertyChecks}

import scala.collection.JavaConverters.asScalaSetConverter

class UtilTests extends FreeSpec with PropertyChecks with TableDrivenPropertyChecks{

  val hexByte = Gen.choose(0, 255).map(n => f"$n%02x")
  val properUUID = Gen.listOfN(16, hexByte).map {
    bytes => Seq(bytes.take(4), bytes.take(2), bytes.take(2), bytes.take(2), bytes.take(6)).map(_.mkString).mkString("-")
  }
  val mushedUUID = Gen.listOfN(16, hexByte).map(_.mkString)
  case class UUIDString(str: String)
  implicit val ArbUUIDString : Arbitrary[UUIDString] = Arbitrary {
    Gen.oneOf(properUUID, mushedUUID) map (UUIDString.apply _)
  }

  "UUIDReads" - {
    "reads valid UUIDs" in {
      val subject = implicitly[Reads[UUID]]
      forAll {
        us: UUIDString =>
          val result = subject(us.str)
          assert(result.isValid)
          result foreach {
            uuid => assert(uuid.toString.replace("-", "") == us.str.replace("-", ""))
          }
      }
    }
  }

  "CurrencyReads" - {
    "reads valid currency codes" in {
      val subject = implicitly[Reads[Currency]]
      Currency.getAvailableCurrencies.asScala foreach {
        c => assert(subject(c.getCurrencyCode) == Valid(c))
      }
    }
  }

}
