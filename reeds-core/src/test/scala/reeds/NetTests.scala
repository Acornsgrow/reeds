package reeds

import java.net.{InetSocketAddress, URI, URL, URLEncoder}

import org.scalatest.FreeSpec
import org.scalatest.prop.{PropertyChecks, TableDrivenPropertyChecks}
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import cats.data.Validated.Valid

class NetTests extends FreeSpec with PropertyChecks with TableDrivenPropertyChecks {

  val genHostname = Gen.listOf(Gen.identifier).map(_.mkString("."))
  val genIpv4Addr = for {
    a <- Gen.choose(0, 255)
    b <- Gen.choose(0, 255)
    c <- Gen.choose(0, 255)
    d <- Gen.choose(0, 255)
  } yield s"$a.$b.$c.$d"

  def withPort(gen: Gen[String]) = for {
    g <- gen
    port <- Gen.choose(1, Short.MaxValue * 2 - 1)
  } yield (g, port)

  val genHost = Gen.oneOf(genHostname, genIpv4Addr)

  val genAuth = for {
    username <- Gen.resize(10, arbitrary[String].map(URLEncoder.encode(_, "UTF-8")))
    password <- Gen.resize(10, arbitrary[String].map(URLEncoder.encode(_, "UTF-8")))
  } yield s"$username:$password"

  val pathPart = Gen.resize(10, arbitrary[String]).map(URLEncoder.encode(_, "UTF-8"))

  val genProtocol =
    Gen.resize(10, Gen.listOf(Gen.frequency((10, Gen.alphaNumChar), (1, Gen.oneOf('-', '+', '.')))))
      .map(_.mkString) suchThat (str => !(Seq('-', '+', '.') contains str.head))

  case class UriString(string: String)

  implicit val arbUri = Arbitrary {
    for {
      protocol <- Gen.oneOf("file", "http", "https", "ftp")
      auth <- Gen.oneOf(Gen.const(""), genAuth map (_ + "@"))
      host <- genHost
      port <- Gen.oneOf(Gen.const(""), Gen.choose(1, Short.MaxValue * 2 - 1).map(":" + _.toString))
      path <- Gen.listOf(pathPart).map(_.mkString("/"))
      query <- Gen.oneOf(Gen.const(""), arbitrary[String].map("?" + URLEncoder.encode(_, "UTF-8")))
      fragment <- Gen.oneOf(Gen.const(""), arbitrary[String].map("#" + URLEncoder.encode(_, "UTF-8")))
    } yield UriString(s"$protocol://$auth$host$port/$path$query$fragment")
  }


  "URLReads" - {
    val subject = implicitly[Reads[URL]]
    "reads valid URLs" in {
      forAll { str: UriString =>
        assert(subject(str.string) == Valid(new URL(str.string)))
      }
    }

    "fails invalid URLs" in {
      assert(subject("not a valid url").isInvalid)
    }
  }

  "URIReads" - {
    val subject = implicitly[Reads[URI]]
    "reads valid URIs" in {
      forAll { str: UriString =>
        assert(subject(str.string) == Valid(new URI(str.string)))
      }
    }

    "fails invalid URIs" in {
      assert(subject("not a valid uri").isInvalid)
    }
  }

  "InetSocketAddressReads" - {
    val subject = implicitly[Reads[InetSocketAddress]]
    "reads valid IPv4 addresses with a port" in {
      forAll(withPort(genIpv4Addr)) {
        case (host, port) =>
          val result = subject(s"$host:$port")
          assert(result.isValid)
          result foreach {
            addr =>
              assert(addr.getHostString == host)
              assert(addr.getPort == port)
          }
      }
    }

    "fails if port is not a number" in {
      assert(subject("host:notanumber").isInvalid)
    }

    "fails if string is not valid" in {
      assert(subject("not an address").isInvalid)
    }
  }


}
