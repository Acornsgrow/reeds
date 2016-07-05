package reeds

import cats.data.Validated.Valid
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import cats.data.Xor
import org.scalatest.FreeSpec
import org.scalatest.prop.{PropertyChecks, TableDrivenPropertyChecks}

class MathTests extends FreeSpec with PropertyChecks with TableDrivenPropertyChecks {

  implicit val arbBD = Arbitrary {
    for {
      whole <- Gen.listOf(Gen.numChar) suchThat (_.nonEmpty)
      frac  <- Gen.listOf(Gen.numChar)
    } yield BigDecimal(whole.mkString + (if(frac.nonEmpty) "." + frac.mkString else ""))
  }

  "BigDecimalReads" - {
    val subject = implicitly[Reads[BigDecimal]]
    "reads BigDecimals" in {
      forAll(arbitrary[BigDecimal]) {
        t =>
          val str = Xor.catchNonFatal(t.bigDecimal.toPlainString)
          str foreach (s => assert(subject(s) == Valid(t)))
      }
    }
    "doesn't read non-decimals" in {
      assert(subject("not a big decimal").isInvalid)
    }
  }

  "BigIntReads" - {
    val subject = implicitly[Reads[BigInt]]
    "reads BigInts" in {
      forAll(arbitrary[BigInt]) {
        t =>
          val str = Xor.catchNonFatal(t.toString)
          str foreach (s => assert(subject(s) == Valid(t)))
      }
    }
    "doesn't read non-ints" in {
      assert(subject("not a big int").isInvalid)
    }
  }

}
