# Contributing

Because reeds intends to be a go-to implementation of `Reads`, we welcome contributions of new instances. To have an
instance included, simply create a pull request that includes your instance(s).

Before creating your pull request, consider the following:
* Are the instances for types which are in the Java or Scala standard library?
   * **Yes**: Include them in `reeds-core`, organized into packages reflecting their package in the standard library.
   * **No**: Does a reeds submodule already exist for the library which contains the types?
      * **Yes**: Include them in the existing submodule.
      * **No**: Is the library that contains the types open source and in wide use?
         * **Yes**: Create a new submodule for the instances
         * **No**: Consider releasing the instances as a separate project for the time being.
* Is there a well-agreed-upon standard way in which the types should be parsed from a string?
   * **Yes**: Perfect!
   * **No**: Reeds doesn't want to be opinionated about its instances.  Go ahead and submit your PR, but don't
     add the instances to the `Reads` companion object; instead, they should be explicitly imported.
* Have you written tests which cover your instances completely?
   * **Yes**: Perfect!
   * **No**: You need to write tests for the instances before submitting your PR.

After making these considerations, go ahead and submit your PR.  We'll review it and, if appropriate, we'll merge it,
increment the version, and make a release!

## Building

To build and test reeds and all of its subprojects,

```
sbt test
```

To run the coverage report:

```
sbt clean coverage test && sbt coverageAggregate
```

To build and test only a single subproject:

```
sbt "project reeds-shapeless" test
```