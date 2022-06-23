package org.combinators.cls.ide.examples.hydrocluster

import org.combinators.cls.inhabitation.InhabitationAlgorithm
import org.combinators.cls.interpreter.{ReflectedRepository, combinator}
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.cls.ide.examples.hydrocluster.Algebraic.{Algebra, AlgebraInstance}
import org.combinators.cls.ide.examples.hydrocluster.SemanticTypes._
import org.combinators.cls.ide.examples.hydrocluster.SynthesisRequest
import shapeless.feat.Enumerable

object ExampleRepo3 {

  trait EnvironmentSig {
    def apply(rows: Seq[Algebra], sensors: Seq[Algebra]): Algebra

    val semanticType: Type = list(row) =>: list(sensor) =>: environment
  }

  trait RowSig {
    def apply(towers: Seq[Algebra]): Algebra

    val semanticType: Type = list(tower) =>: row
  }

  trait TowerSig {
    def apply(base: Algebra, body: Seq[Algebra], top: Algebra): Algebra

    val semanticType: Type = base =>: towerBody =>: top =>: tower
  }

  //Überflüssig??!
  trait StapleSig {
    def apply(combination: Seq[Algebra]): Seq[Algebra]

    val semanticType: Type = towerCombination =>: towerBody
  }

  trait StackbaseSig {
    def apply(sensors: Seq[Algebra]): Algebra

    val semanticType: Type = list(sensor) =>: base
  }

  trait SimpleTopSig {
    def apply(sensors: Seq[Algebra]): Algebra

    val semanticType: Type = list(sensor) =>: top
  }

  /*  trait SensorInstanceSig {
      def apply(name: Algebra, sensorTyp: Algebra): Algebra
      val semanticType: Type = sensorName =>: sensorType =>: sensor
    }*/

  trait HoneyCombLayerSig {
    //def apply(tray1: Algebra, tray2: Algebra, tray3: Algebra): Algebra
    def apply(tray: Algebra): Algebra

    //val semanticType: Type = honeyCombTray =>: honeyCombTray =>: honeyCombTray =>: honeyCombLayer
    val semanticType: Type = honeyCombTray =>: honeyCombLayer
  }

  trait TurbineLayerSig {
    //def apply(tray1: Algebra, tray2: Algebra, tray3: Algebra): Algebra
    def apply(tray: Algebra): Algebra

    //val semanticType: Type = turbineTray =>: turbineTray =>: turbineTray =>: turbineLayer
    val semanticType: Type = turbineTray =>: turbineLayer
  }

  trait VineLayerSig {
    def apply(sensors: Seq[Algebra]): Algebra

    val semanticType: Type = list(sensor) :&: vine =>: vineLayer
  }

  object HoneyCombLayer extends HoneyCombLayerSig {
    //override def apply(tray1: Algebra, tray2: Algebra, tray3: Algebra): Algebra = new Algebra {
    override def apply(tray: Algebra): Algebra = new Algebra {
      override def result(algI: AlgebraInstance): algI.A = {
        val trays = Seq(tray, tray, tray)
        algI.honeycomblayer(trays.map(tray => tray.result(algI)), Seq())
      }
    }
  }

  object TurbineLayer extends TurbineLayerSig {
    //override def apply(tray1: Algebra, tray2: Algebra, tray3: Algebra): Algebra = new Algebra {
    override def apply(tray: Algebra): Algebra = new Algebra {
      override def result(algI: AlgebraInstance): algI.A = {
        val trays = Seq(tray, tray, tray)
        algI.turbinelayer(trays.map(tray => tray.result(algI)), Seq())
      }
    }
  }

  object VineLayer extends VineLayerSig {
    override def apply(sensors: Seq[Algebra]): Algebra = new Algebra {
      override def result(algI: AlgebraInstance): algI.A = {
        algI.vinelayer(sensors.map(sensor => sensor.result(algI)))
      }
    }
  }

  trait HoneyCombTraySig {
    def apply(sensors: Seq[Algebra]): Algebra

    val semanticType: Type = list(sensor) :&: honeyComb =>: honeyCombTray
  }

  trait TurbineTraySig {
    def apply(sensors: Seq[Algebra]): Algebra

    val semanticType: Type = list(sensor) :&: turbine =>: turbineTray
  }

  trait SensorListSig {
    def apply(sensor: Algebra): Seq[Algebra]

    val semanticType: Type = sensor =>: list(sensor)
  }

  /*
    trait SensorListSig {
      def apply(sensor: Algebra, sensorlist: Seq[Algebra]): Seq[Algebra]

      val semanticType: Type = sensor =>: list(sensor) =>: list(sensor)
    }
    */
  trait FixedSensorListSig {
    def apply(): Seq[Algebra]

    val semanticType: Type = list(sensor) :&: beta
  }

  trait VineSensorListSig {
    def apply(): Seq[Algebra]

    val semanticType: Type = list(sensor) :&: vine
  }

  trait TurbineSensorListSig {
    def apply(): Seq[Algebra]

    val semanticType: Type = list(sensor) :&: turbine
  }

  trait HoneyCombSensorListSig {
    def apply(): Seq[Algebra]

    val semanticType: Type = list(sensor) :&: honeyComb
  }

  object DHT22SensorList extends FixedSensorListSig {
    override def apply(): Seq[Algebra] = Seq(new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.sensor("Dht22", Seq("Temperature", "Humidity"))
    })
  }

  object DHT22VineSensorList extends VineSensorListSig {
    override def apply(): Seq[Algebra] = Seq(new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.sensor("Dht22", Seq("Temperature", "Humidity"))
    })
  }

  object DHT22TurbineSensorList extends TurbineSensorListSig {
    override def apply(): Seq[Algebra] = Seq(new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.sensor("Dht22", Seq("Temperature", "Humidity"))
    })
  }

  object DHT22HoneyCombSensorList extends HoneyCombSensorListSig {
    override def apply(): Seq[Algebra] = Seq(new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.sensor("Dht22", Seq("Temperature", "Humidity"))
    })
  }

  //Entspricht eigentlich dem Term:
  // Sensor(Dht22, (humidity::temperature::Nil))
  trait DHT22SensorSig {
    def apply(): Algebra

    val semanticType: Type = sensor :&: humidity :&: temperature
  }

  object layers {

    sealed trait Layer

    sealed trait LayerVTH extends Layer

    sealed trait LayerVT extends Layer

    sealed trait LayerVH extends Layer

    sealed trait LayerTH extends Layer

    case class HoneyComb() extends LayerVTH with LayerVH with LayerTH

    case class Turbine() extends LayerVTH with LayerVT with LayerTH

    case class Vine() extends LayerVTH with LayerVT with LayerVH

  }

  //Variation
  /*
  Eine Variation nimmt einen Int length für ihre Länge und einen Int index für den Index des length-Tuples, das der Variation
  von Layern für den TowerBody entspricht.
  */

  object Variation0 {
    def apply(): Seq[Algebra] = Nil

    val semanticType: Type = towerCombination
  }

  sealed case class VariationVTH(length: Int, index: Int) {
    assert(length > 0)

    val layerCombination: List[layers.LayerVTH] = {
      def computeEnumIndex(length: Int, index: Int): Int = length match {
        case 0 => index
        case n => computeEnumIndex(n - 1, index) + scala.math.pow(3, n - 1).toInt
      }

      val layerEnum: Enumerable[List[layers.LayerVTH]] = Enumerable[List[layers.LayerVTH]]

      layerEnum.enumerate.index(computeEnumIndex(length, index))
    }

    def vineCount(combination: List[layers.LayerVTH]): Int = {
      combination.foldLeft(0)({
        case (n, layers.Vine()) => n + 1
        case (n, _) => n
      })
    }

    def turbineCount(combination: List[layers.LayerVTH]): Int = {
      combination.foldLeft(0)({
        case (n, layers.Turbine()) => n + 1
        case (n, _) => n
      })
    }

    def honeycombCount(combination: List[layers.LayerVTH]): Int = {
      combination.foldLeft(0)({
        case (n, layers.HoneyComb()) => n + 1
        case (n, _) => n
      })
    }

    def apply(honeyCombLayers: Seq[Algebra], turbineLayers: Seq[Algebra], vineLayers: Seq[Algebra]): Seq[Algebra] = {
      //val layerCombination = computeLayerCombination()
      layerCombination.foldLeft((Seq[(layers.LayerVTH, Int)](), (0, 0, 0)))({
        case (b, layers.Vine()) => (b._1 :+ (layers.Vine(), b._2._1), (b._2._1 + 1, b._2._2, b._2._3))
        case (b, layers.Turbine()) => (b._1 :+ (layers.Turbine(), b._2._2), (b._2._1, b._2._2 + 1, b._2._3))
        case (b, layers.HoneyComb()) => (b._1 :+ (layers.HoneyComb(), b._2._3), (b._2._1, b._2._2, b._2._3 + 1))
      })._1.map {
        case (layers.HoneyComb(), index) => honeyCombLayers.apply(index)
        case (layers.Turbine(), index) => turbineLayers.apply(index)
        case (layers.Vine(), index) => vineLayers.apply(index)
      }
    }

    val semanticType: Type = vec(honeycombCount(layerCombination), honeyCombLayer) =>: vec(turbineCount(layerCombination), turbineLayer) =>: vec(vineCount(layerCombination), vineLayer) =>: towerCombination
  }

  sealed case class VariationVT(length: Int, index: Int) {
    assert(length > 0)

    val layerCombination: List[layers.LayerVT] = {
      def computeEnumIndex(length: Int, index: Int): Int = length match {
        case 0 => index
        case n => computeEnumIndex(n - 1, index) + scala.math.pow(2, n - 1).toInt
      }

      val layerEnum: Enumerable[List[layers.LayerVT]] = Enumerable[List[layers.LayerVT]]

      layerEnum.enumerate.index(computeEnumIndex(length, index))
    }

    def vineCount(combination: List[layers.LayerVT]): Int = {
      combination.foldLeft(0)({
        case (n, layers.Vine()) => n + 1
        case (n, _) => n
      })
    }

    def turbineCount(combination: List[layers.LayerVT]): Int = {
      combination.foldLeft(0)({
        case (n, layers.Turbine()) => n + 1
        case (n, _) => n
      })
    }

    /*
    def honeycombCount(combination: List[layers.LayerVT]): Int = {
      combination.foldLeft(0)({
        case (n, layers.HoneyComb()) => n+1
        case (n, _ ) => n
      })
    }
     */

    def apply(turbineLayers: Seq[Algebra], vineLayers: Seq[Algebra]): Seq[Algebra] = {
      //val layerCombination = computeLayerCombination()
      layerCombination.foldLeft((Seq[(layers.LayerVT, Int)](), (0, 0, 0)))({
        case (b, layers.Vine()) => (b._1 :+ (layers.Vine(), b._2._1), (b._2._1 + 1, b._2._2, b._2._3))
        case (b, layers.Turbine()) => (b._1 :+ (layers.Turbine(), b._2._2), (b._2._1, b._2._2 + 1, b._2._3))
        //case (b, layers.HoneyComb()) => (b._1 :+ (layers.HoneyComb(),b._2._3), (b._2._1, b._2._2, b._2._3+1))
      })._1.map {
        //case (layers.HoneyComb(), index) => honeyCombLayers.apply(index)
        case (layers.Turbine(), index) => turbineLayers.apply(index)
        case (layers.Vine(), index) => vineLayers.apply(index)
      }
    }

    val semanticType: Type = vec(turbineCount(layerCombination), turbineLayer) =>: vec(vineCount(layerCombination), vineLayer) =>: towerCombination
  }

  sealed case class VariationVH(length: Int, index: Int) {
    assert(length > 0)

    val layerCombination: List[layers.LayerVH] = {
      def computeEnumIndex(length: Int, index: Int): Int = length match {
        case 0 => index
        case n => computeEnumIndex(n - 1, index) + scala.math.pow(2, n - 1).toInt
      }

      val layerEnum: Enumerable[List[layers.LayerVH]] = Enumerable[List[layers.LayerVH]]

      layerEnum.enumerate.index(computeEnumIndex(length, index))
    }

    def vineCount(combination: List[layers.LayerVH]): Int = {
      combination.foldLeft(0)({
        case (n, layers.Vine()) => n + 1
        case (n, _) => n
      })
    }

    /*
        def turbineCount(combination: List[layers.LayerVH]): Int = {
          combination.foldLeft(0)({
            case (n, layers.Turbine()) => n+1
            case (n, _ ) => n
          })
        }
    */

    def honeycombCount(combination: List[layers.LayerVH]): Int = {
      combination.foldLeft(0)({
        case (n, layers.HoneyComb()) => n + 1
        case (n, _) => n
      })
    }


    def apply(honeyCombLayers: Seq[Algebra], vineLayers: Seq[Algebra]): Seq[Algebra] = {
      //val layerCombination = computeLayerCombination()
      layerCombination.foldLeft((Seq[(layers.LayerVH, Int)](), (0, 0, 0)))({
        case (b, layers.Vine()) => (b._1 :+ (layers.Vine(), b._2._1), (b._2._1 + 1, b._2._2, b._2._3))
        //case (b, layers.Turbine()) => (b._1 :+ (layers.Turbine(),b._2._2), (b._2._1, b._2._2+1, b._2._3))
        case (b, layers.HoneyComb()) => (b._1 :+ (layers.HoneyComb(), b._2._3), (b._2._1, b._2._2, b._2._3 + 1))
      })._1.map {
        case (layers.HoneyComb(), index) => honeyCombLayers.apply(index)
        //case (layers.Turbine(), index) => turbineLayers.apply(index)
        case (layers.Vine(), index) => vineLayers.apply(index)
      }
    }

    val semanticType: Type = vec(honeycombCount(layerCombination), honeyCombLayer) =>: vec(vineCount(layerCombination), vineLayer) =>: towerCombination
  }

  sealed case class VariationTH(length: Int, index: Int) {
    assert(length > 0)

    val layerCombination: List[layers.LayerTH] = {
      def computeEnumIndex(length: Int, index: Int): Int = length match {
        case 0 => index
        case n => computeEnumIndex(n - 1, index) + scala.math.pow(2, n - 1).toInt
      }

      val layerEnum: Enumerable[List[layers.LayerTH]] = Enumerable[List[layers.LayerTH]]

      layerEnum.enumerate.index(computeEnumIndex(length, index))
    }
    /*
        def vineCount(combination: List[layers.LayerTH]): Int = {
          combination.foldLeft(0)({
            case (n, layers.Vine()) => n+1
            case (n, _ ) => n
          })
        }
     */

    def turbineCount(combination: List[layers.LayerTH]): Int = {
      combination.foldLeft(0)({
        case (n, layers.Turbine()) => n + 1
        case (n, _) => n
      })
    }

    def honeycombCount(combination: List[layers.LayerTH]): Int = {
      combination.foldLeft(0)({
        case (n, layers.HoneyComb()) => n + 1
        case (n, _) => n
      })
    }

    def apply(honeyCombLayers: Seq[Algebra], turbineLayers: Seq[Algebra]): Seq[Algebra] = {
      //val layerCombination = computeLayerCombination()
      layerCombination.foldLeft((Seq[(layers.LayerTH, Int)](), (0, 0, 0)))({
        //case (b, layers.Vine()) => (b._1 :+ (layers.Vine(),b._2._1), (b._2._1+1, b._2._2, b._2._3))
        case (b, layers.Turbine()) => (b._1 :+ (layers.Turbine(), b._2._2), (b._2._1, b._2._2 + 1, b._2._3))
        case (b, layers.HoneyComb()) => (b._1 :+ (layers.HoneyComb(), b._2._3), (b._2._1, b._2._2, b._2._3 + 1))
      })._1.map {
        case (layers.HoneyComb(), index) => honeyCombLayers.apply(index)
        case (layers.Turbine(), index) => turbineLayers.apply(index)
        //case (layers.Vine(), index) => vineLayers.apply(index)
      }
    }

    val semanticType: Type = vec(honeycombCount(layerCombination), honeyCombLayer) =>: vec(turbineCount(layerCombination), turbineLayer) =>: towerCombination
  }

  sealed case class VineTowerCombination(length: Int) {
    def apply(layers: Seq[Algebra]): Seq[Algebra] = {
      layers
    }

    val semanticType: Type = vec(length, vineLayer) =>: towerCombination
  }

  sealed case class TurbineTowerCombination(length: Int) {
    def apply(layers: Seq[Algebra]): Seq[Algebra] = {
      layers
    }

    val semanticType: Type = vec(length, turbineLayer) =>: towerCombination
  }

  sealed case class HoneyCombTowerCombination(length: Int) {
    def apply(layers: Seq[Algebra]): Seq[Algebra] = {
      layers
    }

    val semanticType: Type = vec(length, honeyCombLayer) =>: towerCombination
  }

  sealed case class Repository(request: SynthesisRequest) {


    def addCombinatorsForRequest(reflectedRepository: ReflectedRepository[Repository]): ReflectedRepository[Repository] = {

      //Add variations for layer height
      //All layers are valid here !!!
      val refRepWithVariation = {
        if (request.height == 0) {
          reflectedRepository.addCombinator(Variation0)
        }
        else {
          val refRep = request.validLayers.foldLeft(reflectedRepository) {
            case (rep, "Vine") => rep.addCombinator(VineLayer)
            case (rep, "Turbine") => rep.addCombinator(TurbineLayer)
            case (rep, "HoneyComb") => rep.addCombinator(HoneyCombLayer)
          }
          val layerVariation: (Boolean, Boolean, Boolean) = (request.validLayers.contains("Vine"),
            request.validLayers.contains("Turbine"), request.validLayers.contains("HoneyComb"))
          layerVariation match {
            //(Vine, Turbine, Honeycomb)
            case (false, false, false) =>
              assert(assertion = true, "There must be a spelling mistake in validLayer list!")
              refRep.addCombinator(Variation0)
            case (false, false, true) => refRep.addCombinator(HoneyCombTowerCombination(request.height))
            case (false, true, false) => refRep.addCombinator(TurbineTowerCombination(request.height))
            case (false, true, true) => 0.until(scala.math.pow(2, request.height).toInt).foldLeft(refRep) { case (rep, n) => rep.addCombinator(VariationTH(request.height, n)) }
            case (true, false, false) => refRep.addCombinator(VineTowerCombination(request.height))
            case (true, false, true) => 0.until(scala.math.pow(2, request.height).toInt).foldLeft(refRep) { case (rep, n) => rep.addCombinator(VariationVH(request.height, n)) }
            case (true, true, false) => 0.until(scala.math.pow(2, request.height).toInt).foldLeft(refRep) { case (rep, n) => rep.addCombinator(VariationVT(request.height, n)) }
            case (true, true, true) => 0.until(scala.math.pow(3, request.height).toInt).foldLeft(refRep) { case (rep, n) => rep.addCombinator(VariationVTH(request.height, n)) }
          }
        }
      }

      //Add sensor combinators
      //DHT22: Humidity & Temperature
      val dht22Variation: (Boolean, Boolean, Boolean) = {
        //(Vine, Turbine, Honeycomb)
        (request.vineSensors.contains("Humidity") && request.vineSensors.contains("Temperature"),
          request.turbineSensors.contains("Humidity") && request.turbineSensors.contains("Temperature"),
          request.honeycombSensors.contains("Humidity") && request.honeycombSensors.contains("Temperature"))
      }

      val refRepWithVariationAndDht22 = dht22Variation match {
        //(Vine, Turbine, Honeycomb)
        case (false, false, false) => refRepWithVariation
        case (false, false, true) => refRepWithVariation.addCombinator(DHT22HoneyCombSensorList)
        case (false, true, false) => refRepWithVariation.addCombinator(DHT22TurbineSensorList)
        case (false, true, true) => refRepWithVariation.addCombinator(DHT22SensorList) //.addCombinator(DHT22HoneyCombSensorList)
        case (true, false, false) => refRepWithVariation.addCombinator(DHT22VineSensorList)
        case (true, false, true) => refRepWithVariation.addCombinator(DHT22SensorList) //.addCombinator(DHT22HoneyCombSensorList)
        case (true, true, false) => refRepWithVariation.addCombinator(DHT22SensorList) //.addCombinator(DHT22TurbineSensorList)
        case (true, true, true) => refRepWithVariation.addCombinator(DHT22SensorList)
      }

      refRepWithVariationAndDht22

    }

    def toReflectedRepository(algo: InhabitationAlgorithm, cl: ClassLoader): ReflectedRepository[Repository] = {

      val reflectedRep = ReflectedRepository(this, TaxonomyKinding.semanticTaxonomy, TaxonomyKinding.basicKinding,
        algorithm = algo, classLoader = cl)

      addCombinatorsForRequest(reflectedRep)
    }

    //noinspection AccessorLikeMethodIsEmptyParen
    def toReflectedRepository(): ReflectedRepository[Repository] = {

      val reflectedRep = ReflectedRepository(this, TaxonomyKinding.semanticTaxonomy, TaxonomyKinding.basicKinding) //, algorithm = algo, classLoader = cl)

      addCombinatorsForRequest(reflectedRep)
    }

    @combinator object LayerList0 {
      def apply(): Seq[Algebra] = Nil

      val semanticType: Type = vec(0, alpha)
    }

    @combinator object LayerList1 {
      def apply(layer1: Algebra): Seq[Algebra] = Seq(layer1)

      val semanticType: Type = alpha =>: vec(1, alpha)
    }

    @combinator object LayerList2 {
      def apply(layer1: Algebra, layer2: Algebra): Seq[Algebra] = Seq(layer1, layer2)

      val semanticType: Type = alpha =>: alpha =>: vec(2, alpha)
    }

    @combinator object LayerList3 {
      def apply(layer1: Algebra, layer2: Algebra, layer3: Algebra): Seq[Algebra] = Seq(layer1, layer2, layer3)

      val semanticType: Type = alpha =>: alpha =>: alpha =>: vec(3, alpha)
    }

    @combinator object LayerList4 {
      def apply(layer1: Algebra, layer2: Algebra, layer3: Algebra, layer4: Algebra): Seq[Algebra] =
        Seq(layer1, layer2, layer3, layer4)

      val semanticType: Type = alpha =>: alpha =>: alpha =>: alpha =>: vec(4, alpha)
    }

    @combinator object LayerList5 {
      def apply(layer1: Algebra, layer2: Algebra, layer3: Algebra, layer4: Algebra, layer5: Algebra): Seq[Algebra] =
        Seq(layer1, layer2, layer3, layer4, layer5)

      val semanticType: Type = alpha =>: alpha =>: alpha =>: alpha =>: alpha =>: vec(5, alpha)
    }

    @combinator object Environment extends EnvironmentSig {
      override def apply(rows: Seq[Algebra], sensors: Seq[Algebra]): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A = {
          algI.environment(rows.map(row => row.result(algI)), sensors.map(sensor => sensor.result(algI)))
        }
      }
    }

    @combinator object Row extends RowSig {
      override def apply(towers: Seq[Algebra]): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A = {
          algI.row(towers.map(tower => tower.result(algI)))
        }
      }
    }

    @combinator object Tower extends TowerSig {
      override def apply(base: Algebra, body: Seq[Algebra], top: Algebra): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A = {
          algI.tower(base.result(algI), body.map(layer => layer.result(algI)), top.result(algI))
        }
      }
    }

    //Überflüssig??!
    @combinator object Staple extends StapleSig {
      override def apply(combination: Seq[Algebra]): Seq[Algebra] = combination
    }

    @combinator object Stackbase extends StackbaseSig {
      override def apply(sensors: Seq[Algebra]): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A = {
          algI.stackbase(sensors.map(sensor => sensor.result(algI)))
        }
      }
    }

    @combinator object SimpleTop extends SimpleTopSig {
      override def apply(sensors: Seq[Algebra]): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A = {
          algI.simpletop(sensors.map(sensor => sensor.result(algI)))
        }
      }
    }

    /*
    @combinator object SensorInstance extends SensorInstanceSig {
      override def apply(name: Algebra, sensorTyp: Algebra): Algebra
    }
    */

    /*@combinator object HoneyCombLayer extends HoneyCombLayerSig {
      override def apply(tray1: Algebra, tray2: Algebra, tray3: Algebra): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A = {
          val trays = Seq(tray1, tray2, tray3)
          algI.honeycomblayer(trays.map(tray => tray.result(algI)))
        }
      }
    }

    @combinator object TurbineLayer extends TurbineLayerSig {
      override def apply(tray1: Algebra, tray2: Algebra, tray3: Algebra): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A = {
          val trays = Seq(tray1, tray2, tray3)
          algI.turbinelayer(trays.map(tray => tray.result(algI)))
        }
      }
    }

    @combinator object VineLayer extends VineLayerSig {
      override def apply(sensors: Seq[Algebra]): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A = {
          algI.vinelayer(sensors.map(sensor => sensor.result(algI)))
        }
      }
    }
*/
    @combinator object HoneyCombTray extends HoneyCombTraySig {
      override def apply(sensors: Seq[Algebra]): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A = {
          algI.honeycombtray(sensors.map(sensor => sensor.result(algI)))
        }
      }
    }

    @combinator object TurbineTray extends TurbineTraySig {
      override def apply(sensors: Seq[Algebra]): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A = {
          algI.turbinetray(sensors.map(sensor => sensor.result(algI)))
        }
      }
    }

    /*
    @combinator object LayerList extends LayerListSig {
      override def apply(layer1: Algebra, layer2: Algebra, layer3: Algebra, layer4: Algebra, layer5: Algebra)
      : Seq[Algebra] = Seq(layer1, layer2, layer3, layer4, layer5)
    }

     */


    /*
        @combinator object SensorList extends SensorListSig {
          override def apply(sensor: Algebra): Seq[Algebra] = Seq(sensor)
        }


        @combinator object SensorList extends SensorListSig {
          override def apply(sensor: Algebra, sensorlist: Seq[Algebra]): Seq[Algebra] = sensor +: sensorlist
        }
        */
    @combinator object EmptySensorList extends FixedSensorListSig {
      override def apply(): Seq[Algebra] = Nil
    }

    @combinator object DHT22Sensor extends DHT22SensorSig {
      override def apply(): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A = {
          algI.sensor("Dht22", Seq("Temperature", "Humidity"))
        }
      }
    }


  }

}
