# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]

## [1.0.5] = 2016-07-01
- Cats to 0.6.0
- Circe to 0.5.0-M2

## [1.0.4] - 2016-04-19
- Changed reeds-circe to use Exported - de-prioritize reeds' instances

## [1.0.3] - 2016-04-16
- Updated circe to 0.4.0 in reeds-circe
- Reprioritized instances in reeds-core (move Reads[String] to low-priority implicit)

## [1.0.2] - 2016-03-25
- Added consideration of default values defined on case classes in `reeds.generic`

## [1.0.1] - 2016-03-24
- Removed intermediate classes from `reeds.generic` (removes the extra `()` for `reads` and `readMap`)
- Refactored `Map` and `F[_]` extractors to their own typeclasses

## [1.0.0] - 2016-03-24
- Initial release