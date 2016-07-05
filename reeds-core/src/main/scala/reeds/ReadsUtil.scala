package reeds

import cats.data.Validated, Validated._


object ReadsUtil {

  /**
    * Create a stream which stops evaluating when a Valid case is reached
    */
  def evalSuccess[I,V](stream: Stream[Validated[I,V]]) : Stream[Validated[I,V]] = stream.headOption match {
    case Some(valid @ Valid(_)) => valid #:: Stream.empty
    case Some(invalid @ Invalid(_)) => invalid #:: evalSuccess(stream.tail)
    case None => Stream.empty
  }

}
