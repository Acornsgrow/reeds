package reeds

import cats.data.Validated.Valid
import cats.instances.list._
import org.scalatest.FreeSpec


class SyntaxTests extends FreeSpec {

  "Reads[T]" in {
    assert(Reads[Int]("10") == Valid(10))
  }

  "FReads[F, T]" in {
    assert(FReads[List, Int](List("10", "20")) == List(Valid(10), Valid(20)))
  }

  "ErrorWrap" - {
    import Reads._
    val err = new Exception("test")
    "no message" in {
      assert(err.wrap == ExternalError(err))
    }
    "message" in {
      assert(err.wrap("wrap") == WrappedError("wrap", err))
    }
  }

}
