package org.combinators.cls.ide.examples.labyrinth

import java.nio.file.{Path, Paths}

import org.combinators.cls.git.ResultLocation
import org.combinators.templating.persistable.Persistable

object Helpers {
  implicit val dummyResultLocation: ResultLocation = ResultLocation(Paths.get("."))
  implicit val dummyPersistable: Persistable.Aux[Unit] = new Persistable {
    def path(elem: Unit): Path = Paths.get(".")
    def rawText(elem: Unit): Array[Byte] = Array.empty
    type T = Unit
  }

}
