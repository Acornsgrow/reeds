package reeds


trait base {
  import Reads._

  implicit val ShortReads = catching[Short](_.toShort)
  implicit val IntReads: Reads[Int] = catching[Int](_.toInt)
  implicit val LongReads = catching[Long](_.toLong)
  implicit val ByteReads = catching[Byte](_.toByte)
  implicit val FloatReads = catching[Float](_.toFloat)
  implicit val DoubleReads = catching[Double](_.toDouble)
  implicit val BoolReads = catching[Boolean](_.toBoolean)
}
