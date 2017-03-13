package reeds

import cats.syntax.either._
import io.circe.{Decoder, DecodingFailure, HCursor, export}, export.Exported

package object circe {

  implicit def exportCirceDecoder[T](implicit reads: Reads[T]) : Exported[Decoder[T]] = Exported(
    reads.toDecoder
  )

  implicit class ReedsToDecoder[T](val reads: Reads[T]) {
    def toDecoder = Decoder.instance[T] {
      cursor => Decoder.decodeString(cursor) flatMap {
        str => reads(str).leftMap {
          failure => DecodingFailure(failure.head.getMessage, cursor.history)
        }.toEither
      }
    }
  }


}