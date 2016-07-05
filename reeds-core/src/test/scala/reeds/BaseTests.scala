package reeds

import cats.data.Validated.Valid
import org.scalacheck.Arbitrary
import Arbitrary._
import org.scalatest.FreeSpec
import org.scalatest.prop.{PropertyChecks, TableDrivenPropertyChecks}

class BaseTests extends FreeSpec with PropertyChecks with TableDrivenPropertyChecks {


  def check[T : Arbitrary : Reads] = {
    val subject = implicitly[Reads[T]]
    forAll(arbitrary[T]) {
      t => assert(subject(t.toString) == Valid(t))
    }
  }

  "ShortReads" - {
    val subject = implicitly[Reads[Short]]
    "reads shorts" in check[Short]
    "doesn't read non-shorts" in {
      assert(subject("foo").isInvalid)
      assert(subject(Int.MaxValue.toString).isInvalid)
    }
  }

  "IntReads" - {
    val subject = implicitly[Reads[Int]]
    "reads ints" in check[Int]

    "doesn't read non-ints" in {
      assert(subject("foo").isInvalid)
      assert(subject(Long.MaxValue.toString).isInvalid)
    }
  }

  "LongReads" - {
    val subject = implicitly[Reads[Long]]
    "reads longs" in check[Long]

    "doesn't read non-longs" in {
      assert(subject("foo").isInvalid)
      assert(subject(Long.MaxValue.toString + "00").isInvalid)
    }
  }

  "ByteReads" - {
    val subject = implicitly[Reads[Byte]]
    "reads bytes" in check[Byte]

    "doesn't read non-bytes" in {
      assert(subject("foo").isInvalid)
      assert(subject((Byte.MaxValue.toInt + 1).toString).isInvalid)
    }
  }

  "FloatReads" - {
    "reads floats" in check[Float]
  }

  "DoubleReads" - {
    "reads doubles" in check[Double]
  }

  "BoolReads" - {
    val subject = implicitly[Reads[Boolean]]
    "reads booleans" in forAll(Table(
      ("string", "expected"),
      ("true", true),
      ("TRUE", true),
      ("True", true),
      ("false", false),
      ("FALSE", false),
      ("False", false)
    )) {
      case (str, expected) => assert(subject(str) == Valid(expected))
    }
  }

  "StringReads" - {
    "reads strings" in check[String]
  }

}
