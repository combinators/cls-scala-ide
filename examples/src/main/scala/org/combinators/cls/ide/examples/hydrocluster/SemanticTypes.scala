package org.combinators.cls.ide.examples.hydrocluster

import org.combinators.cls.types.{Constructor, Type, Variable}

object SemanticTypes {
  lazy val sensorType: Constructor = Constructor("sensor_type")
  lazy val temperature: Constructor = Constructor("temperature")
  lazy val humidity: Constructor = Constructor("humidity")
  lazy val uvLevel: Constructor = Constructor("uvLevel")
  lazy val sealevel: Constructor = Constructor("seaLevel")
  lazy val water: Constructor = Constructor("water")
  lazy val moisture: Constructor = Constructor("moisture")
  lazy val pressure: Constructor = Constructor("pressure")
  lazy val altitude: Constructor = Constructor("altitude")

  lazy val sensorName: Constructor = Constructor("sensor_name")
  lazy val dht22: Constructor = Constructor("dht22")
  lazy val bmp280: Constructor = Constructor("bmp280")
  lazy val bmp085: Constructor = Constructor("bmp58")
  lazy val veml6070: Constructor = Constructor("veml6070")
  lazy val mcp3008: Constructor = Constructor("mcp3008")

  lazy val digital: Constructor = Constructor("digital")

  //-------------Sorts------------------
  lazy val term: Constructor = Constructor("term")

  lazy val environment: Constructor = Constructor("environment")
  lazy val row: Constructor = Constructor("row")
  lazy val tower: Constructor = Constructor("tower")
  lazy val sensor: Constructor = Constructor("sensor")
  lazy val actuator: Constructor = Constructor("actuator")
  lazy val sensorAndActuator: Constructor = Constructor("sensorAndActuator")
  lazy val base: Constructor = Constructor("base")
  lazy val top: Constructor = Constructor("top")
  lazy val tray: Constructor = Constructor("tray")
  lazy val towerCombination: Constructor = Constructor("tower_combination")
  lazy val towerBody: Constructor = Constructor("tower_body")
  lazy val layer: Constructor = Constructor("layer")
  lazy val simpleTop: Constructor = Constructor("simple_top")
  lazy val stackbase: Constructor = Constructor("stackbase")
  lazy val honeyCombLayer: Constructor = Constructor("honeycomb_layer")
  lazy val turbineLayer: Constructor = Constructor("turbine_layer")
  lazy val vineLayer: Constructor = Constructor("vine_layer")
  lazy val doubleVineLayer: Constructor = Constructor("doubleVine_layer")
  lazy val honeyCombTray: Constructor = Constructor("honeycomb_tray")
  lazy val turbineTray: Constructor = Constructor("turbine_tray")
  lazy val vine: Constructor = Constructor("vine")
  lazy val doubleVine: Constructor = Constructor("double_vine")
  lazy val turbine: Constructor = Constructor("turbine")
  lazy val honeyComb: Constructor = Constructor("honeycomb")
  //Variable for kind of layer
  lazy val alpha: Variable = Variable("alpha")
  //Variable for kind of design
  lazy val beta: Variable = Variable("beta")
  //Variable for only sensors, actuators or both as an argument
  lazy val gamma: Variable = Variable("gamma")


  //------------------------------------

  def vec(length: Int, elem: Type): Constructor = Constructor("Vec(" + length.toString + ")", list(elem))

  def list(elem: Type): Constructor = Constructor("List", elem)

}
