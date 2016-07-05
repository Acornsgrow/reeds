# Reeds [![Build Status](https://travis-ci.com/Acornsgrow/reeds.svg?token=YBfF1eSQ3q7xjqPgpdEw&branch=master)](https://travis-ci.com/Acornsgrow/reeds)

Reeds is a small Scala library with one goal: provide a standard `Reads[T]` typeclass and standard instances for it.

1. [Reads[T]](#reads-t)
2. [Usage](#usage)
3. [Included instances](#included-instances)
4. [Defining instances](#defining-instances)
5. [Subprojects](#subprojects)
   1. [`reeds-shapeless`](#reeds-shapeless)
   2. [`reeds-circe`](#reeds-circe)
6. [Contributing](#contributing)
7. [Code of conduct](#code-of-conduct)
8. [License](#license)
9. [Contributors](#contributors)

## `Reads[T]`

Most projects will at one point need to read simple values from strings.  In Scala, this often ends up as some variation
of a typeclass that captures the operation `String => T` (or `String => F[T]` for some functor `F[_]`). The goal of
Reeds is to provide this typeclass as an alternative to proliferating one-off isomorphisms of it.

`Reads` is defined thusly:

```scala
trait Reads[T] extends (String => ValidatedNel[Throwable, T])
```

`ValidatedNel` comes from [cats](https://github.com/typelevel/cats) and captures a validated value, which is either a
value or a list of errors.  `Validated` is different from `Xor` in that it is typically used to aggregate all 
failures, rather than short-circuiting at the first failure.

## Usage

To simply read a value of type `T` from a `String`:

```scala
val validResult = Reads.apply[UUID]("deadbeef-dead-dead-beef-deaddeadbeef")
//validResult: Valid[UUID]

val invalidResult = Reads.apply[UUID]("not a valid UUID")
//invalidResult: Invalid[Throwable]
```

This requires that an instance `Reads[T]` is available (in the above example, `Reads[UUID]`).

A lifted typeclass `FReads[F[_], T]` is also provided.  This allows any `F[String]` to be read into 
`F[ValidatedNel[Throwable, T]]` if `F` has a `Functor` instance and `T` has a `Reads` instance:

```scala
val results = FReads.apply[Seq, UUID](Seq("deadbeef-dead-dead-beef-deaddeadbeef", "not a valid UUID"))
//results: Seq[ValidatedNel[Throwable, UUID]]

val result = results.sequenceU
//result: Invalid[Throwable]
```

## Included instances

Several commonly-used instances are provided on the `Reads` companion object.  It is done this way so that you (and
users of your library) don't have to know about Reeds or import a bunch of junk from Reeds to use your code.  This
goes against the somewhat common practice of requiring users (and users' users, and users' users' users etc) to import
the instances into scope.

The instances currently provided are:

* `Short`, `Int`, `Long`, `Byte`, `Float`, `Double`, `Bool` - these are pretty self-explanatory; each delegates to `_.toX`
* `BigDecimal`, `BigInt` - delegate to the built-in parser for each.
* `java.net.URL`, `java.net.URI` - these behave pretty much as you would expect - by simply passing the string to
  `new URL(_)` and `new URI(_)`.
* `java.net.InetSocketAddress` - this instance expects a hostname or address (IPv4 or [IPv6]) and a port number, with
  a colon separating the host and port.
* `java.util.UUID` - this instance attempts to delegate the UUID parsing to `UUID.fromString`, but falls back to 
  stripping all dashes and separating the digits into the format that `UUID.fromString` expects (8-4-4-4-12).
* `java.util.Currency` - delegates to `java.util.Currency.getInstance(_.toUpperCase)`
* `java.time.LocalDate`, `java.time.LocalDateTime` - expect a date or date/time, respectively, each in ISO-8601 format
  without a time zone.
* `java.time.OffsetDateTime`, `java.time.ZonedDateTime`, `java.time.Instant`, `java.util.Date` - all expect a date/time 
  in ISO-8601 format with a timezone offset.  In the case of `java.time.Instant`, the given result is the `Instant` at
  UTC for the given time with its zone (i.e. the same result as `OffsetDateTime` with `_.toInstant` applied).
* `java.sql.Date` - expects a date in common SQL format `YYYY-MM-DD` (which happily coincides with ISO-8601 for dates).
* `java.sql.Timestamp` - expects a date/time in common SQL format `YYYY-MM-DD HH:mm:ss.ms`.  Delegates to
  `java.sql.Timestamp.valueOf(_)`.

## Defining instances

If you need instances that aren't provided, use `Reads.instance`, i.e.:

```scala
import cats.data.Validated, Validated._
implicit val MyTypeReads : Reads[MyType] = Reads.instance[MyType] {
  str => if(str == "foo") Valid(MyType()) else Invalid(Reads.SimpleError(s"$str is not a valid MyType")).toValidatedNel
}
```

## Subprojects

`Reads` by itself isn't all that interesting; it's what you can do with it as a typeclass that make it useful.  For
example, take a look at the two tiny subprojects:

### reeds-shapeless

`reeds-shapeless` can derive conversions from `String` products to any `case class` which has a `Reads` instance for
each of its product types.  For example, I could define an ADT:

```scala
case class Sale(id: UUID, createdAt: OffsetDateTime, amount: BigDecimal, currency: Currency)
```

And if I have a list of strings (say, from a CSV or command line arguments) I can automatically marshall it:

```scala
import reeds.generic._
val result = Seq(
  "deadbeef-dead-beef-beef-deaddeadbeef",
  "2016-03-25T07:17:27.209-0700",
  "237.99",
  "USD"
).reads[Sale]
  
//result: Valid(Sale(...))
```

Similarly, if I have a `Map[String, String]`, I can marshall that as well:

```scala
val result = Map(
  "id" -> "deadbeef-dead-beef-beef-deaddeadbeef",
  "createdAt" -> "2016-03-25T07:17:27.209-0700",
  "amount" -> "237.99",
  "currency" -> "USD"
).readMap[Sale]
  
//result: Valid(Sale(...))
```

In that case, my ADT can also include `Option`, and will pass validation even if optional fields are missing from the map:

```scala
case class SaleOptional(id: UUID, createdAt: LocalDate, amount: Option[BigDecimal], currency: Currency)

val result = Map(
  "id" -> "deadbeef-dead-beef-beef-deaddeadbeef",
  "createdAt" -> "2016-03-25T07:17:27.209-0700",
  "currency" -> "USD"
).readMap[SaleOptional]
  
//result: Valid(SaleOptional(deadbeef-dead-beef-beef-deaddeadbeef, 2016-03-25T07:17:27.209-0700, None, USD))
```

Reeds also knows about default values, so if I have an ADT with defaults:

```scala
case class SaleDefault(id: UUID, createdAt: LocalDate, amount: BigDecimal, currency: Currency = Currency.getInstance("USD"))
```

Then it will be provided if the input doesn't specify it:

```scala
val result = Map(
  "id" -> "deadbeef-dead-beef-beef-deaddeadbeef",
  "createdAt" -> "2016-03-25T07:17:27.209-0700",
  "amount" -> "237.99"
).readMap[SaleOptional]

//result: Valid(Sale(..., USD))
```

Finally, if I have a `Map[String, F[String]]` for some `Functor[F]`, I can have that F embedded in my ADT:

```scala
import cats.std.list._

case class SaleList(id: UUID, createdAt: LocalDate, amounts: List[BigDecimal], currency: Currency)

val result = Map(
  "id" -> List("deadbeef-dead-beef-beef-deaddeadbeef"),
  "createdAt" -> List("2016-03-25T07:17:27.209-0700"),
  "amounts" -> List("220.99", "42.22", "47.47"),
  "currency" -> List("USD")).readMap[SaleList]
  
//result: Valid(SaleList(deadbeef-dead-beef-beef-deaddeadbeef, 2016-03-25T07:17:27.209-0700, List(220.99, 42.22, 47.47), USD))
```

### reeds-circe
`reeds-circe` provides a (low-priority) circe `Decoder` for anything that has a `Reads` instance.  This gives you a
free `Decoder` for all the `java.time` types, `UUID`, `URI`, and `URL`, assuming that they are typically stored as
strings in JSON.  Also, if you happen to define a custom `Reads`, you'll get a free `Decoder` for it as well.

```scala
import reeds.circe._
import circe.generic.auto._
import java.time.OffsetDateTime

case class Foo(id: Int, createdAt: OffsetDateTime)

val result = circe.parser.parse("""{"id": 10, "createdAt": "2016-03-25T07:17:27.209-0700"}""").as[Foo]

//result: Right(Foo(10, 2016-03-25T07:17:27.209-0700))
```

## Contributing
Because reeds intends to be a go-to implementation of `Reads`, we welcome contributions of new instances. Please see the
[contribution guide](CONTRIBUTING.md) for more information about how to contribute to reeds.

## Code of conduct
The reeds project supports the [Typelevel Code of Conduct](http://typelevel.org/conduct.html) and wants all its channels
to be welcoming environments for everyone.

## License
Reeds is released under the [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)

    Copyright 2016 Acorns Grow, Inc.
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at
    
      http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
    
## Contributors
* [jeremyrsmith](https://github.com/jeremyrsmith)