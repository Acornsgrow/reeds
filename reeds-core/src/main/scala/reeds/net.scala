package reeds

import java.net.{InetSocketAddress, URI, URL}

import cats.data._
import Validated._
import cats.std.list._
import cats.syntax.functor._

trait net {
  import Reads._

  implicit object InetSocketAddressReads extends Reads[InetSocketAddress] {
    //Regex intended to parse host/port combo (host can be a valid hostname, or an IPv4 or IPv6 address)
    private val addressParser = """^([^\:]*|\[(?:[a-fA-F0-9]{0,4}?\:)*[a-fA-F0-9]{1,4}\])\:([0-9]+)$""".r

    def apply(str: String) = str match {
      case addressParser(host, port) =>
        IntReads.apply(port).map {
          portNum => new InetSocketAddress(host, portNum)
        }

      case other => Invalid(SimpleError(s"$other could not be parsed as host:port")).toValidatedNel
    }
  }

  implicit val URLReads = Reads.catching(new URL(_))
  implicit val URIReads = Reads.catching(new URI(_))


}