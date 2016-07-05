package reeds

import java.time._
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.util.Date

import cats.data._
import Validated._

trait time {
  import Reads._
  import ReadsUtil._

  private def localDateTimeFormat() = {
    new DateTimeFormatterBuilder()
      .parseCaseInsensitive()
      .append(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
  }

  implicit object LocalDateTimeReads extends Reads[LocalDateTime] {
    private lazy val format = localDateTimeFormat().toFormatter
    def apply(str: String) = Validated.catchNonFatal {
      LocalDateTime.parse(str, format)
    }.leftMap(_.wrap).toValidatedNel
  }

  private object ZonedTimeUtils {

    // Formats are done as a stream so that they don't have to be evaluated unless used
    private val formats = Stream(
      ("+HHMMss","Z"),
      ("+HH:MM:ss","Z"),
      ("+HHmm","Z"),
      ("+HH:mm","Z"),
      ("+HH","Z"))

    private lazy val formatters = formats map {
      case (f, z) => localDateTimeFormat().appendOffset(f,z).toFormatter
    }

    def withFormatters[T](f: (String, DateTimeFormatter) => T) : (String => ValidatedNel[Throwable, T]) = (str: String) => {
      val attempts = evalSuccess(formatters.map {
        formatter =>
          Validated.catchNonFatal(f(str, formatter))
      })

      attempts
        .lastOption
        .map(_.leftMap(_.wrap))
        // $COVERAGE-OFF$This code is unreachable
        .getOrElse(Invalid(SimpleError(s"Could not parse $str as ISO-8601 timestamp")))
        // $COVERAGE-ON$
        .toValidatedNel
    }

  }

  implicit val ZonedDateTimeReads : Reads[ZonedDateTime] =
    Reads.instance[ZonedDateTime](ZonedTimeUtils.withFormatters(ZonedDateTime.parse(_, _)))

  implicit val OffsetDateTimeReads : Reads[OffsetDateTime] =
    Reads.instance[OffsetDateTime](ZonedTimeUtils.withFormatters(OffsetDateTime.parse(_, _)))

  implicit val InstantReads = OffsetDateTimeReads.map(_.toInstant)

  implicit val LocalDateReads : Reads[LocalDate] = Reads.catching[LocalDate](LocalDate.parse(_))

  implicit val YearMonthReads : Reads[YearMonth] = Reads.catching[YearMonth](YearMonth.parse(_))

  implicit val DateReads : Reads[Date] = InstantReads.map(Date.from)

  implicit val SqlDateReads : Reads[java.sql.Date] = Reads.catching[java.sql.Date](java.sql.Date.valueOf)

  implicit val SqlTimestampReads : Reads[java.sql.Timestamp] =
    Reads.catching[java.sql.Timestamp](java.sql.Timestamp.valueOf)
}