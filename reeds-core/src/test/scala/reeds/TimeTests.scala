package reeds

import java.time._
import java.util.Date

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import cats.data.Validated
import org.scalatest.FreeSpec
import org.scalatest.prop.{PropertyChecks, TableDrivenPropertyChecks}


class TimeTests extends FreeSpec with PropertyChecks with TableDrivenPropertyChecks{

  case class TestDate(year: Int, month: Int, day: Int) {
    override def toString = f"$year-$month%02d-$day%02d"
  }
  case class TestTime(hour: Int, minute: Int, second: Int, nano: Int) {
    override def toString = f"$hour%02d:$minute%02d:$second%02d.$nano%09d"
  }
  case class TestLocalDateTime(date: TestDate, time: TestTime) {
    override def toString = s"${date.toString}T${time.toString}"
  }
  case class TestDateTime(dateTime: TestLocalDateTime, offset: TestOffset) {
    override def toString = s"${dateTime.toString}${offset.toString}"
  }

  trait TestOffset {
    def totalSeconds : Int
  }
  case class RealTestOffset(hour: Int, minute: Int, second: Int, specificity: Int, separator: String) extends TestOffset {
    override def toString = {
      val pieces = Seq(hour, minute, second).take(specificity).map(num => if(num < 0) f"$num%03d" else f"$num%02d")
      val str = pieces.mkString(separator)
      if(hour >= 0) "+" + str else str
    }
    val sign = if(hour < 0) -1 else 1
    def totalSeconds = Seq(hour * 60 * 60, minute * 60 * sign, second * sign).take(specificity).sum
  }
  case object ZOffset extends TestOffset {
    override def toString = "Z"
    def totalSeconds = 0
  }

  // only 4-digit years are supported by java.time
  val arbYear = Gen.oneOf(Gen.choose(-9999, -1000), Gen.choose(1000, 9999))
  val arbMonth = Gen.choose(1, 12)
  val arbDay = Gen.choose(1, 28)
  val arbHour = Gen.choose(0, 23)
  val arbMinute = Gen.choose(0, 59)
  val arbSecond = arbMinute
  val arbNano = Gen.choose(0, 999999999)
  val arbOffsetHour = Gen.choose(-17, 17)

  val arbOffsetTime = for {
    specificity <- Gen.choose(1, 3)
    separator <- Gen.oneOf("", ":")
    hour <- arbOffsetHour
    minute <- arbMinute
    second <- arbSecond
  } yield RealTestOffset(hour, minute, second, specificity, separator)

  val arbOffset = Gen.oneOf(arbOffsetTime, Gen.const(ZOffset))

  implicit val arbTestDate : Arbitrary[TestDate] = Arbitrary {
    for {
      year <- arbYear
      month <- arbMonth
      day <- arbDay
    } yield TestDate(year, month, day)
  }

  implicit val arbTestTime : Arbitrary[TestTime] = Arbitrary {
    for {
      hour <- arbHour
      minute <- arbMinute
      second <- arbSecond
      nano <- arbNano
    } yield TestTime(hour, minute, second, nano)
  }

  implicit val arbLocalDateTime : Arbitrary[TestLocalDateTime] = Arbitrary {
    for {
      date <- arbitrary[TestDate]
      time <- arbitrary[TestTime]
    } yield TestLocalDateTime(date, time)
  }

  implicit val arbDateTime : Arbitrary[TestDateTime] = Arbitrary {
    for {
      dateTime <- arbitrary[TestLocalDateTime]
      offset <- arbOffset
    } yield TestDateTime(dateTime, offset)
  }

  // I say structural types are OK in tests
  def assertSameDate[T <: { def getYear() : Int; def getMonthValue() : Int; def getDayOfMonth() : Int} , U](v: Validated[U, T], date: TestDate) = {
    assert(v.isValid)
    v foreach { valid =>
      assert(valid.getYear == date.year)
      assert(valid.getMonthValue == date.month)
      assert(valid.getDayOfMonth == date.day)
    }
  }

  def assertSameTime[T <: { def getHour() : Int; def getMinute() : Int; def getSecond() : Int; def getNano() : Int }, U](v: Validated[U, T], time: TestTime) = {
    assert(v.isValid)
    v foreach { result =>
      assert(result.getHour == time.hour)
      assert(result.getMinute == time.minute)
      assert(result.getSecond == time.second)
      assert(result.getNano == time.nano)
    }
  }

  "LocalDateReads" - {
    val subject = implicitly[Reads[LocalDate]]
    "reads ISO-8601 dates" in {
      forAll {
        date: TestDate =>
          assertSameDate(subject(date.toString), date)
      }
    }
    "doesn't read non-dates" in {
      assert(subject("not a date").isInvalid)
    }
  }

  "LocalDateTimeReads" - {
    val subject = implicitly[Reads[LocalDateTime]]
    "reads ISO-8601 timestamps without zones" in {
      forAll {
        time: TestLocalDateTime =>
          val maybeResult = subject(time.toString)
          assertSameDate(maybeResult, time.date)
          assertSameTime(maybeResult, time.time)
      }
    }
    "doesn't read non-dates" in {
      assert(subject("not a date").isInvalid)
    }
  }

  "ZonedDateTimeReads" - {
    val subject = implicitly[Reads[ZonedDateTime]]
    "reads ISO-8601 timestamps with zones" in {
      forAll {
        time: TestDateTime =>
          val maybeResult = subject(time.toString)
          assertSameDate(maybeResult, time.dateTime.date)
          assertSameTime(maybeResult, time.dateTime.time)
          maybeResult foreach {
            result => assert(result.getOffset.getTotalSeconds == time.offset.totalSeconds)
          }
      }
    }
    "doesn't read non-dates" in {
      assert(subject("not a date").isInvalid)
    }
  }

  "OffsetDateTimeReads" - {
    val subject = implicitly[Reads[OffsetDateTime]]
    "reads ISO-8601 timestamps with zones" in {
      forAll {
        time: TestDateTime =>
          val maybeResult = subject(time.toString)
          assertSameDate(maybeResult, time.dateTime.date)
          assertSameTime(maybeResult, time.dateTime.time)
          maybeResult foreach {
            result => assert(result.getOffset.getTotalSeconds == time.offset.totalSeconds)
          }
      }
    }
    "doesn't read non-dates" in {
      assert(subject("not a date").isInvalid)
    }
  }

  "InstantReads" - {
    val subject = implicitly[Reads[Instant]]
    "reads ISO-8601 timestamps with zones" in {
      forAll {
        time: TestDateTime =>
          val maybeResult = subject(time.toString) map (_.atOffset(ZoneOffset.ofTotalSeconds(time.offset.totalSeconds)))
          assertSameDate(maybeResult, time.dateTime.date)
          assertSameTime(maybeResult, time.dateTime.time)
          maybeResult foreach {
            result => assert(result.getOffset.getTotalSeconds == time.offset.totalSeconds)
          }
      }
    }
    "doesn't read non-dates" in {
      assert(subject("not a date").isInvalid)
    }
  }

  "YearMonthReads" - {
    val subject = implicitly[Reads[YearMonth]]
    "reads yyyy-mm" in {
      forAll {
        date: TestDate =>
          val str = f"${date.year}-${date.month}%02d"
          val result = subject(str)
          assert(result.isValid)
          result foreach { d =>
            assert(d.getYear == date.year)
            assert(d.getMonthValue == date.month)
          }
      }
    }
    "doesn't read non-dates" in {
      assert(subject("not a date").isInvalid)
    }
  }

  "DateReads" - {
    val subject = implicitly[Reads[Date]]
    "reads ISO-8601 timestamps with zones" in {
      forAll {
        time: TestDateTime =>
          val maybeResult = subject(time.toString) map (_.toInstant.atOffset(ZoneOffset.ofTotalSeconds(time.offset.totalSeconds)))
          assertSameDate(maybeResult, time.dateTime.date)
          assertSameTime(maybeResult, time.dateTime.time.copy(nano = (time.dateTime.time.nano / 1000000) * 1000000))
      }
    }
    "doesn't read non-dates" in {
      assert(subject("not a date").isInvalid)
    }
  }

  "SqlDateReads" - {
    val subject = implicitly[Reads[java.sql.Date]]
    "reads yyyy-mm-dd" in {
      forAll(arbitrary[TestDate] suchThat (_.year >= 1970)) { // sql date and timestamp can only handle 1970 and up
        date : TestDate =>
          val maybeResult = subject(date.toString).map(_.toLocalDate)
          assertSameDate(maybeResult, date)
      }
    }
    "doesn't read non-dates" in {
      assert(subject("not a date").isInvalid)
    }
  }

  "SqlTimestampReads" - {
    val subject = implicitly[Reads[java.sql.Timestamp]]
    "reads yyyy-mm-dd hh:ii:ss.mmm" in {
      forAll(arbitrary[TestLocalDateTime] suchThat (_.date.year >= 1970)) {
        time : TestLocalDateTime =>
          val str = f"${time.date.toString} ${time.time.hour}%02d:${time.time.minute}%02d:${time.time.second}%02d.${time.time.nano / 1000000}%03d"
          val maybeResult = subject(str)
          assert(maybeResult.isValid)
      }
    }
    "doesn't read non-dates" in {
      assert(subject("not a date").isInvalid)
    }
  }

}
