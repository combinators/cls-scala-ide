
package org.combinators.cls.ide.examples.temperature

import org.combinators.cls.types._
import org.combinators.cls.types.syntax._

trait SemanticTypes {

  // whenever you want a Generic (like in java) you use variable with kinding
  val unitType      = Variable("UnitType")
  val units:Kinding = Kinding(unitType)
    .addOption(unit.celsius)
    .addOption(unit.fahrenheit)
    .addOption(unit.kelvin)

  val precisionType = Variable("PrecisionType")
  val precisions:Kinding = Kinding(precisionType)
    .addOption(precision.floating)
    .addOption(precision.integer)

  // whenever you want a subtype, use Taxonomies
  val taxonomyLoss:Taxonomy = Taxonomy(precision.lossyPrecision.toString).
    addSubtype(precision.fullPrecision.toString)

  object unit {
    def apply (part:Type):Type = 'Unit(part)

    val celsius:Type    = 'Celsius
    val fahrenheit:Type = 'Fahrenheit
    val kelvin:Type     = 'Kelvin
  }

  object precision {
    def apply (part:Type):Type = 'Precision(part)

    val fullPrecision:Type = 'Full
    val lossyPrecision:Type = 'Lossy
    val integer: Type = 'Integer
    val floating:Type = 'Float
  }

  object artifact {
    def apply (part:Type):Type = 'Artifact(part)

    val api:Type       = 'WeatherAPI
    val impl:Type      = 'Impl
    val compute:Type   = 'Compute
    val location:Type  = 'Location
  }
}
