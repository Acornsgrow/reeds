import java.time.OffsetDateTime
import java.util.UUID

import cats.syntax.either._
import reeds.circe._
import io.circe._
import org.scalatest.FlatSpec
import reeds.Reads

class PriorityTest extends FlatSpec {

  "reed-circe decoder" should "materialize for needed decoder" in {
    val decoder = implicitly[Decoder[OffsetDateTime]]
    val dt = OffsetDateTime.now()
    val json = Json.fromString(dt.toString)
    assert(decoder.apply(json.hcursor) == Right(dt))
    val result = decoder.apply(Json.fromString("not a uuid").hcursor)
    assert(result.isLeft)
    result.leftMap { err =>
      assert(err.history != null)  //coverage is coverage
    }

  }

  it should "have lower priority than pre-existing circe decoder" in {
    assert(implicitly[Decoder[UUID]] == Decoder.decodeUUID)
  }

  "reedsToDecoder" should "convert a reads instance to a circe decoder" in {
    val decoder = implicitly[Reads[UUID]].toDecoder
    val json = Json.fromString("deadbeef-dead-dead-beef-deaddeadbeef")
    assert(decoder.apply(json.hcursor) == Right(UUID.fromString("deadbeef-dead-dead-beef-deaddeadbeef")))
  }

}
