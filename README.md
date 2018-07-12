# cls-scala-ide
[![Maven Central](https://img.shields.io/maven-central/v/org.combinators/cls-scala-ide_2.12.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.combinators%22%20AND%20%22cls-scala-ide%22)
[![build status](https://travis-ci.com/combinators/cls-scala-ide.svg?branch=master)](https://travis-ci.com/combinators/cls-scala-ide)
[![Coverage Status](https://coveralls.io/repos/github/combinators/cls-scala-ide/badge.svg?branch=master)](https://coveralls.io/github/combinators/cls-scala-ide?branch=master)
[![Join the chat at https://gitter.im/combinators/cls-scala](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/combinators/cls-scala)
## IDE for the Combinatory Logic Synthesizer (CL)S Framework

This project implements an IDE for the Combinatory Logic Synthesizer (CL)S framework.

For more information see our [documentation project](https://combinators.github.io/).

Existing users please refer to the [CHANGELOG](releases) for news.

## Installation

Add the following dependency to your existing sbt project: 
```scala
libraryDependencies += "org.combinators" %% "cls-scala-ide" % "<VERSION>"
```
The string `<VERSION>` has to be replaced by the version you want.
You can search for released versions [here](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.combinators%22%20AND%20a%3A%22cls-scala-ide%22).

To obtain the latest unreleased development version, clone the repository and run `sbt publishLocal`.

Currently, Scala 2.11 and 2.12 are supported.

## Examples

Can be found in the [examples project](examples/src/main/scala/org/combinators/cls/ide/examples) and 
the [tests](src/test/scala/org/combinators/cls/ide).

##### Got a new example?
If it is contained within one file, consider adding it to ours: 
join the [chat](https://gitter.im/combinators/cls-scala) or just open a pull request.

## Help and Contributions

Join [combinators/cls-scala](https://gitter.im/combinators/cls-scala) on Gitter.

### Main Authors

- Anna Vasileva
- Jan Bessai

### Contributers

-
##### Your name here?
Just the usual: open pull requests and or issues.
Feel free to add yourself to the list in this file, if you contributed something.
