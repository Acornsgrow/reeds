package reeds


trait math {

  import Reads._

  implicit val BigDecimalReads : Reads[BigDecimal] = Reads.catching(BigDecimal(_))
  implicit val BigIntReads : Reads[BigInt] = Reads.catching(BigInt(_))

}
