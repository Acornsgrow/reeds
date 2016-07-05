package reeds

import java.util.{Currency, UUID}

import cats.data.Validated
import cats.data.Validated._

trait util {
  import Reads._

  implicit val UUIDReads : Reads[UUID] = Reads.instance[UUID] {
    str =>
      //UUID#fromString requires a specific format
      (Validated.catchNonFatal {
        UUID.fromString(str)
      } orElse {
        val stripped = str.toLowerCase.replaceAll("""[^a-z0-9]""", "")
        if(stripped.length != 32)
          Invalid(Reads.SimpleError(s"'$str' is not a valid UUID."))
        else {
          val built = Seq(
            stripped.substring(0, 8),
            stripped.substring(8, 12),
            stripped.substring(12, 16),
            stripped.substring(16, 20),
            stripped.substring(20, 32)
          )
          Validated.catchNonFatal(UUID.fromString(built.mkString("-")))
        }
      }).toValidatedNel
  }

  implicit val CurrencyReads: Reads[Currency] = Reads.catching(str => Currency.getInstance(str.toUpperCase))

}
