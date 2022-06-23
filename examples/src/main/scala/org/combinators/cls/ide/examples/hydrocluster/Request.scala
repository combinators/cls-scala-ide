package org.combinators.cls.ide.examples.hydrocluster

/**
  * A generic Request must always specify how its corresponding RequestHandler is to be constructed, and it must have a
  * request type, e.g. metamodel or gui etc.
  */


//TO-DO: Refactor Names to be sane

//GUI * means make full JSON
final case class SynthesisRequest(
  name: String,
  var reqType: String,
  height: Int, //amount of layers excluding base and top * possible
  vineSensors: List[String], //"Humidity"."Temperature, Pressure, Altitude, Sealevel, Uvlevel,Water, SoilMoisture, * bad idea
  turbineSensors: List[String],
  honeycombSensors: List[String],
  towerActuators: List[String], //Can contain "Pump" and "Lamp"
  validLayers: List[String], //Combination of HoneyComb, Turbine, Vine/ GUI fakes *
  topSensors: List[String]
)

