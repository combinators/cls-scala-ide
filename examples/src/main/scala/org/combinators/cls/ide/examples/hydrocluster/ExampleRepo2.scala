package org.combinators.cls.ide.examples.hydrocluster

import org.combinators.cls.inhabitation.InhabitationAlgorithm
import org.combinators.cls.interpreter.{ReflectedRepository, combinator}
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.cls.ide.examples.hydrocluster.Algebraic.{Algebra, AlgebraInstance}
import org.combinators.cls.ide.examples.hydrocluster.SemanticTypes._
import org.combinators.cls.ide.examples.hydrocluster.SynthesisRequest
import shapeless.feat.Enumerable

object ExampleRepo2 {

  trait EnvironmentSig {
    val semanticType: Type = list(row) =>: list(sensor) =>: environment

    def apply(rows: Seq[Algebra], sensors: Seq[Algebra]): Algebra
  }

  trait RowSig {
    val semanticType: Type = list(tower) =>: row

    def apply(towers: Seq[Algebra]): Algebra
  }

  trait TowerSig {
    val semanticType: Type = base =>: towerCombination =>: top =>: tower

    def apply(base: Algebra, body: Seq[Algebra], top: Algebra): Algebra
  }

  //Überflüssig??!
  trait StapleSig {
    val semanticType: Type = towerCombination =>: towerBody

    def apply(combination: Seq[Algebra]): Seq[Algebra]
  }

  trait StackbaseSig {
    val semanticType: Type = base // list(sensor) =>: base

    def apply(): Algebra //(sensors: Seq[Algebra]): Algebra
  }

  trait SimpleTopSig {
    val semanticType: Type = list(sensor) :&: simpleTop =>: top

    def apply(sensors: Seq[Algebra]): Algebra
  }

  /*  trait SensorInstanceSig {
      def apply(name: Algebra, sensorTyp: Algebra): Algebra
      val semanticType: Type = sensorName =>: sensorType =>: sensor
    }*/

  trait HoneyCombLayerSig {
    //val semanticType: Type = honeyCombTray =>: honeyCombTray =>: honeyCombTray =>: honeyCombLayer
    val semanticType: Type = honeyCombTray =>: list(sensorAndActuator) :&: honeyComb :&: layer =>: honeyCombLayer

    //def apply(tray1: Algebra, tray2: Algebra, tray3: Algebra): Algebra
    def apply(tray: Algebra, sensors: Seq[Algebra]): Algebra
  }

  trait TurbineLayerSig {
    //val semanticType: Type = turbineTray =>: turbineTray =>: turbineTray =>: turbineLayer
    val semanticType: Type = turbineTray =>: list(sensorAndActuator) :&: turbine :&: layer =>: turbineLayer

    //def apply(tray1: Algebra, tray2: Algebra, tray3: Algebra): Algebra
    def apply(tray: Algebra, sensors: Seq[Algebra]): Algebra
  }

  trait VineLayerSig {
    val semanticType: Type = list(sensorAndActuator) :&: vine :&: layer =>: vineLayer

    def apply(sensors: Seq[Algebra]): Algebra
  }

  trait DoubleVineLayerSig {
    val semanticType: Type = list(sensorAndActuator) :&: doubleVine :&: layer =>: doubleVineLayer

    def apply(sensors: Seq[Algebra]): Algebra
  }

  trait HoneyCombTraySig {
    val semanticType: Type = list(sensor) :&: honeyComb :&: tray =>: honeyCombTray

    def apply(sensors: Seq[Algebra]): Algebra
  }

  trait TurbineTraySig {
    val semanticType: Type = list(sensor) :&: turbine :&: tray =>: turbineTray

    def apply(sensors: Seq[Algebra]): Algebra
  }

  trait SensorListSig {
    val semanticType: Type = sensor =>: list(sensor)

    def apply(sensor: Algebra): Seq[Algebra]
  }

  /*
  trait SensorListSig {
    def apply(sensor: Algebra, sensorlist: Seq[Algebra]): Seq[Algebra]

    val semanticType: Type = sensor =>: list(sensor) =>: list(sensor)
  }
   */
  trait FixedSensorListSig {
    val semanticType: Type = list(sensor) :&: beta :&: tray

    def apply(): Seq[Algebra]
  }

  trait FixedActuatorListSig {
    val semanticType: Type = list(actuator) :&: beta :&: tray

    def apply(): Seq[Algebra]
  }

  trait FixedSensorAndActuatorListSig {
    val semanticType: Type = list(sensorAndActuator) :&: beta :&: tray

    def apply(): Seq[Algebra]
  }

  trait FixedLayerArgumentsListSig {
    val semanticType: Type = list(gamma) :&: beta :&: layer

    def apply(): Seq[Algebra]
  }

  trait VineSensorListSig {
    val semanticType: Type = list(sensorAndActuator) :&: vine :&: layer

    def apply(): Seq[Algebra]
  }

  trait SimpleTopSensorListSig {
    val semanticType: Type = list(sensor) :&: simpleTop :&: layer

    def apply(): Seq[Algebra]
  }

  trait TurbineSensorListSig {
    val semanticType: Type = list(sensor) :&: turbine :&: tray

    def apply(): Seq[Algebra]
  }

  trait HoneyCombSensorListSig {
    val semanticType: Type = list(sensor) :&: honeyComb :&: tray

    def apply(): Seq[Algebra]
  }

  //Entspricht eigentlich dem Term:
  // Sensor(Dht22, (humidity::temperature::Nil))
  trait DHT22SensorSig {
    val semanticType: Type = sensor :&: humidity :&: temperature

    def apply(): Algebra
  }

  final case class DoubleVine()

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

    def computeDoubleVines(combination: Seq[layers.LayerVTH]): Seq[Either[layers.LayerVTH, DoubleVine]] = combination match {
      case layers.Vine() +: layers.Vine() +: xs => Left(layers.Vine()) +: Right(DoubleVine()) +: computeDoubleVines(xs)
      case x +: xs => Left(x) +: computeDoubleVines(xs)
      case Nil => Nil
    }

    def vineCount(combination: Seq[Either[layers.LayerVTH, DoubleVine]]): Int =
      combination.foldLeft(0)({
        case (n, Left(layers.Vine())) => n + 1
        case (n, _) => n
      })

    def doubleVineCount(combination: Seq[Either[layers.LayerVTH, DoubleVine]]): Int =
      combination.foldLeft(0)({
        case (n, Right(DoubleVine())) => n + 1
        case (n, _) => n
      })

    def turbineCount(combination: List[layers.LayerVTH]): Int =
      combination.foldLeft(0)({
        case (n, layers.Turbine()) => n + 1
        case (n, _) => n
      })

    def honeycombCount(combination: List[layers.LayerVTH]): Int =
      combination.foldLeft(0)({
        case (n, layers.HoneyComb()) => n + 1
        case (n, _) => n
      })

    val semanticType: Type = vec(honeycombCount(layerCombination), honeyCombLayer) =>:
      vec(turbineCount(layerCombination), turbineLayer) =>:
      vec(vineCount(computeDoubleVines(layerCombination)), vineLayer) =>:
      vec(doubleVineCount(computeDoubleVines(layerCombination)), doubleVineLayer) =>:
      towerCombination

    /*
        def vineCount(combination: Seq[layers.LayerVTH]): Int =
          combination.foldLeft(0)({
            case (n, layers.Vine()) => n + 1
            case (n, _)             => n
          })

     */

    def apply(honeyCombLayers: Seq[Algebra], turbineLayers: Seq[Algebra], vineLayers: Seq[Algebra], doubleVineLayers: Seq[Algebra]): Seq[Algebra] =
    //val layerCombination = computeLayerCombination()
      computeDoubleVines(layerCombination)
        .foldLeft((Seq[(Either[layers.LayerVTH, DoubleVine], Int)](), (0, 0, 0, 0)))(
          {
            case (b, Left(layers.Vine())) => (b._1 :+ (Left(layers.Vine()), b._2._1), (b._2._1 + 1, b._2._2, b._2._3, b._2._4))
            case (b, Right(DoubleVine())) => (b._1 :+ (Right(DoubleVine()), b._2._4), (b._2._1, b._2._2, b._2._3, b._2._4 + 1))
            case (b, Left(layers.Turbine())) => (b._1 :+ (Left(layers.Turbine()), b._2._2), (b._2._1, b._2._2 + 1, b._2._3, b._2._4))
            case (b, Left(layers.HoneyComb())) => (b._1 :+ (Left(layers.HoneyComb()), b._2._3), (b._2._1, b._2._2, b._2._3 + 1, b._2._4))
          }
        )
        ._1
        .map {
          case (Left(layers.HoneyComb()), index) => honeyCombLayers.apply(index)
          case (Left(layers.Turbine()), index) => turbineLayers.apply(index)
          case (Left(layers.Vine()), index) => vineLayers.apply(index)
          case (Right(DoubleVine()), index) => doubleVineLayers.apply(index)
        }
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

    def computeDoubleVines(combination: Seq[layers.LayerVT]): Seq[Either[layers.LayerVT, DoubleVine]] = combination match {
      case layers.Vine() +: layers.Vine() +: xs => Left(layers.Vine()) +: Right(DoubleVine()) +: computeDoubleVines(xs)
      case x +: xs => Left(x) +: computeDoubleVines(xs)
      case Nil => Nil
    }

    //val layerCombination: List[layers.LayerVT] = computeDoubleVines(preLayerCombination)
    def vineCount(combination: Seq[Either[layers.LayerVT, DoubleVine]]): Int =
      combination.foldLeft(0)({
        case (n, Left(layers.Vine())) => n + 1
        case (n, _) => n
      })

    def doubleVineCount(combination: Seq[Either[layers.LayerVT, DoubleVine]]): Int =
      combination.foldLeft(0)({
        case (n, Right(DoubleVine())) => n + 1
        case (n, _) => n
      })

    def turbineCount(combination: List[layers.LayerVT]): Int =
      combination.foldLeft(0)({
        case (n, layers.Turbine()) => n + 1
        case (n, _) => n
      })

    val semanticType: Type = //vec(honeycombCount(layerCombination), honeyCombLayer) =>:
      vec(turbineCount(layerCombination), turbineLayer) =>:
        vec(vineCount(computeDoubleVines(layerCombination)), vineLayer) =>:
        vec(doubleVineCount(computeDoubleVines(layerCombination)), doubleVineLayer) =>:
        towerCombination


    /*
        def honeycombCount(combination: List[layers.LayerVTH]): Int =
          combination.foldLeft(0)({
            case (n, layers.HoneyComb()) => n + 1
            case (n, _)                  => n
          })
     */

    def apply(turbineLayers: Seq[Algebra], vineLayers: Seq[Algebra], doubleVineLayers: Seq[Algebra]): Seq[Algebra] =
    //val layerCombination = computeLayerCombination()
      computeDoubleVines(layerCombination)
        .foldLeft((Seq[(Either[layers.LayerVT, DoubleVine], Int)](), (0, 0, 0)))(
          {
            case (b, Left(layers.Vine())) => (b._1 :+ (Left(layers.Vine()), b._2._1), (b._2._1 + 1, b._2._2, b._2._3))
            case (b, Right(DoubleVine())) => (b._1 :+ (Right(DoubleVine()), b._2._3), (b._2._1, b._2._2, b._2._3 + 1))
            case (b, Left(layers.Turbine())) => (b._1 :+ (Left(layers.Turbine()), b._2._2), (b._2._1, b._2._2 + 1, b._2._3))
            //case (b, layers.HoneyComb()) => (b._1 :+ (layers.HoneyComb(), b._2._3), (b._2._1, b._2._2, b._2._3 + 1))
          }
        )
        ._1
        .map {
          //case (layers.HoneyComb(), index) => honeyCombLayers.apply(index)
          case (Left(layers.Turbine()), index) => turbineLayers.apply(index)
          case (Left(layers.Vine()), index) => vineLayers.apply(index)
          case (Right(DoubleVine()), index) => doubleVineLayers.apply(index)
        }
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

    def computeDoubleVines(combination: Seq[layers.LayerVH]): Seq[Either[layers.LayerVH, DoubleVine]] = combination match {
      case layers.Vine() +: layers.Vine() +: xs => Left(layers.Vine()) +: Right(DoubleVine()) +: computeDoubleVines(xs)
      case x +: xs => Left(x) +: computeDoubleVines(xs)
      case Nil => Nil
    }

    //val layerCombination: List[layers.LayerVH] = computeDoubleVines(preLayerCombination)
    def vineCount(combination: Seq[Either[layers.LayerVH, DoubleVine]]): Int =
      combination.foldLeft(0)({
        case (n, Left(layers.Vine())) => n + 1
        case (n, _) => n
      })

    def doubleVineCount(combination: Seq[Either[layers.LayerVH, DoubleVine]]): Int =
      combination.foldLeft(0)({
        case (n, Right(DoubleVine())) => n + 1
        case (n, _) => n
      })

    /*
        def turbineCount(combination: List[layers.LayerVTH]): Int =
          combination.foldLeft(0)({
            case (n, layers.Turbine()) => n + 1
            case (n, _)                => n
          })
     */

    def honeycombCount(combination: List[layers.LayerVH]): Int =
      combination.foldLeft(0)({
        case (n, layers.HoneyComb()) => n + 1
        case (n, _) => n
      })

    val semanticType: Type = vec(honeycombCount(layerCombination), honeyCombLayer) =>:
      //vec(turbineCount(layerCombination), turbineLayer) =>:
      vec(vineCount(computeDoubleVines(layerCombination)), vineLayer) =>:
      vec(doubleVineCount(computeDoubleVines(layerCombination)), doubleVineLayer) =>:
      towerCombination


    def apply(honeyCombLayers: Seq[Algebra], vineLayers: Seq[Algebra], doubleVineLayers: Seq[Algebra]): Seq[Algebra] =
    //val layerCombination = computeLayerCombination()
      computeDoubleVines(layerCombination)
        .foldLeft((Seq[(Either[layers.LayerVH, DoubleVine], Int)](), (0, 0, 0)))(
          {
            case (b, Left(layers.Vine())) => (b._1 :+ (Left(layers.Vine()), b._2._1), (b._2._1 + 1, b._2._2, b._2._3))
            case (b, Right(DoubleVine())) => (b._1 :+ (Right(DoubleVine()), b._2._2), (b._2._1, b._2._2 + 1, b._2._3))
            //case (b, Left(layers.Turbine()))   => (b._1 :+ (Left(layers.Turbine()), b._2._2), (b._2._1, b._2._2 + 1, b._2._3))
            case (b, Left(layers.HoneyComb())) => (b._1 :+ (Left(layers.HoneyComb()), b._2._3), (b._2._1, b._2._2, b._2._3 + 1))
          }
        )
        ._1
        .map {
          case (Left(layers.HoneyComb()), index) => honeyCombLayers.apply(index)
          //case (Left(layers.Turbine()), index)   => turbineLayers.apply(index)
          case (Left(layers.Vine()), index) => vineLayers.apply(index)
          case (Right(DoubleVine()), index) => doubleVineLayers.apply(index)
        }
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
    def turbineCount(combination: List[layers.LayerTH]): Int =
      combination.foldLeft(0)({
        case (n, layers.Turbine()) => n + 1
        case (n, _) => n
      })

    def honeycombCount(combination: List[layers.LayerTH]): Int =
      combination.foldLeft(0)({
        case (n, layers.HoneyComb()) => n + 1
        case (n, _) => n
      })

    val semanticType: Type = vec(honeycombCount(layerCombination), honeyCombLayer) =>: vec(
      turbineCount(layerCombination),
      turbineLayer
    ) =>: towerCombination


    def apply(honeyCombLayers: Seq[Algebra], turbineLayers: Seq[Algebra]): Seq[Algebra] =
    //val layerCombination = computeLayerCombination()
      layerCombination
        .foldLeft((Seq[(layers.LayerTH, Int)](), (0, 0, 0)))(
          {
            //case (b, layers.Vine()) => (b._1 :+ (layers.Vine(),b._2._1), (b._2._1+1, b._2._2, b._2._3))
            case (b, layers.Turbine()) => (b._1 :+ (layers.Turbine(), b._2._2), (b._2._1, b._2._2 + 1, b._2._3))
            case (b, layers.HoneyComb()) => (b._1 :+ (layers.HoneyComb(), b._2._3), (b._2._1, b._2._2, b._2._3 + 1))
          }
        )
        ._1
        .map {
          case (layers.HoneyComb(), index) => honeyCombLayers.apply(index)
          case (layers.Turbine(), index) => turbineLayers.apply(index)
          //case (layers.Vine(), index) => vineLayers.apply(index)
        }
  }

  sealed case class VineTowerCombination(length: Int) {
    val semanticType: Type = vec(length, vineLayer) =>: towerCombination

    def apply(layers: Seq[Algebra]): Seq[Algebra] =
      layers
  }

  def mergeSeqs(doubles: Seq[Algebra], vines: Seq[Algebra]): Seq[Algebra] = (doubles, vines) match {
    case (d +: ds, v +: vs) => v +: d +: mergeSeqs(ds, vs)
    case (Nil, vs) => vs
    case (ds, Nil) => ds
    case _ => Nil
  }

  sealed case class DoubleVineTowerCombination(length: Int) {
    val semanticType: Type = vec(length / 2, vineLayer) =>: vec(length / 2, doubleVineLayer) =>: towerCombination

    def apply(vineLayers: Seq[Algebra], doubleVineLayers: Seq[Algebra]): Seq[Algebra] = {

      mergeSeqs(doubleVineLayers, vineLayers)
    }
  }

  sealed case class VineAndDoubleVineTowerCombination(length: Int) {
    val semanticType: Type = vec(length % 2 + length / 2, vineLayer) =>: vec(length / 2, doubleVineLayer) =>: towerCombination

    def apply(vines: Seq[Algebra], doubleVines: Seq[Algebra]): Seq[Algebra] =
      mergeSeqs(doubleVines, vines)
  }

  sealed case class TurbineTowerCombination(length: Int) {
    val semanticType: Type = vec(length, turbineLayer) =>: towerCombination

    def apply(layers: Seq[Algebra]): Seq[Algebra] =
      layers
  }

  sealed case class HoneyCombTowerCombination(length: Int) {
    val semanticType: Type = vec(length, honeyCombLayer) =>: towerCombination

    def apply(layers: Seq[Algebra]): Seq[Algebra] =
      layers
  }

  sealed case class Repository(request: SynthesisRequest) {

    def toReflectedRepository(algo: InhabitationAlgorithm, cl: ClassLoader): ReflectedRepository[Repository] = {

      val reflectedRep = ReflectedRepository(
        this,
        TaxonomyKinding.semanticTaxonomy,
        TaxonomyKinding.basicKinding,
        algorithm = algo,
        classLoader = cl
      )

      addCombinatorsForRequest(reflectedRep)
    }

    def addCombinatorsForRequest(
                                  reflectedRepository: ReflectedRepository[Repository]
                                ): ReflectedRepository[Repository] = {

      //Add variations for layer height
      //All layers are valid here !!!
      val refRepWithVariation = {
        if (request.height == 0) {
          reflectedRepository.addCombinator(Variation0)
        } else {
          val refRep = request.validLayers.foldLeft(reflectedRepository) {
            case (rep, "Vine") => rep.addCombinator(VineLayer).addCombinator(DoubleVineLayer)
            case (rep, "Turbine") => rep.addCombinator(TurbineLayer)
            case (rep, "HoneyComb") => rep.addCombinator(HoneyCombLayer)
            case (rep, _) => rep
          }
          val layerVariation: (Boolean, Boolean, Boolean) = (
            request.validLayers.contains("Vine"),
            request.validLayers.contains("Turbine"),
            request.validLayers.contains("HoneyComb")
          )
          layerVariation match {
            //(Vine, Turbine, Honeycomb)
            case (false, false, false) =>
              assert(assertion = true, "There must be a spelling mistake in validLayer list!")
              refRep.addCombinator(Variation0)
            case (false, false, true) => refRep.addCombinator(HoneyCombTowerCombination(request.height))
            case (false, true, false) => refRep.addCombinator(TurbineTowerCombination(request.height))
            case (false, true, true) =>
              0.until(scala.math.pow(2, request.height).toInt).foldLeft(refRep) {
                case (rep, n) => rep.addCombinator(VariationTH(request.height, n))
              }
            case (true, false, false) =>
              if (request.height % 2 == 0) {
                refRep.addCombinator(DoubleVineTowerCombination(request.height))
              }
              else if (request.height == 1) {
                refRep.addCombinator(VineTowerCombination(request.height))
              }
              else {
                refRep.addCombinator(VineAndDoubleVineTowerCombination(request.height))
              }
            case (true, false, true) =>
              0.until(scala.math.pow(2, request.height).toInt).foldLeft(refRep) {
                case (rep, n) => rep.addCombinator(VariationVH(request.height, n))
              }
            case (true, true, false) =>
              0.until(scala.math.pow(2, request.height).toInt).foldLeft(refRep) {
                case (rep, n) => rep.addCombinator(VariationVT(request.height, n))
              }
            case (true, true, true) =>
              0.until(scala.math.pow(3, request.height).toInt).foldLeft(refRep) {
                case (rep, n) => rep.addCombinator(VariationVTH(request.height, n))
              }
          }
        }
      }

      //Add sensor combinators

      def typeToDescription(sensorList: Seq[String]): Seq[(String, Seq[String])] = {
        var dummyList: Seq[(String, Seq[String])] = Seq()
        if (sensorList.contains("Humidity") || sensorList.contains("Temperature")) {
          dummyList = ("Dht22", Seq("Humidity", "Temperature")) +: dummyList
        }
        if (sensorList.contains("Uvlevel")) {
          dummyList = ("Bh1750", Seq("Uvlevel")) +: dummyList
        }
        if (sensorList.contains("Pressure") || sensorList.contains("Altitude")) {
          dummyList = ("Bmp280", Seq("Pressure", "Altitude")) +: dummyList
        }
        /*
        if (sensorList.contains("Water")) {
          dummyList = ("AnalogueWater", Seq("Water")) +: dummyList
        }
         */
        if (sensorList.contains("SoilMoisture")) {
          dummyList = ("AnalogueMoisture", Seq("SoilMoisture")) +: dummyList
        }

        dummyList
      }

      def typeToDescriptionForVine(sensorList: Seq[String]): Seq[(String, Seq[String])] = {
        var dummyList: Seq[(String, Seq[String])] = Seq()
        if (sensorList.contains("Humidity") || sensorList.contains("Temperature")) {
          dummyList = ("Dht22", Seq("Humidity", "Temperature")) +: dummyList
        }
        if (sensorList.contains("Uvlevel")) {
          dummyList = ("Bh1750", Seq("Uvlevel")) +: dummyList
        }
        if (sensorList.contains("Pressure") || sensorList.contains("Altitude")) {
          dummyList = ("Bmp280", Seq("Pressure", "Altitude")) +: dummyList
        }
        if (sensorList.contains("Water")) {
          dummyList = ("AnalogueWater", Seq("Water")) +: dummyList
        }
        if (sensorList.contains("SoilMoisture")) {
          dummyList = ("AnalogueMoisture", Seq("SoilMoisture")) +: dummyList
        }

        dummyList
      }

      //Add sensor combinators
      val vineSensorDescription = typeToDescriptionForVine(request.vineSensors)
      val honeycombSensorCombDescription = typeToDescription(request.honeycombSensors)
      val turbineSensorDescription = typeToDescription(request.turbineSensors)
      val simpleTopSensorDescription = typeToDescription(request.topSensors)

      val vineCombinations = 1.to(vineSensorDescription.length).map(i => vineSensorDescription.combinations(i))
      val refRepWithVine = vineCombinations.foldLeft(refRepWithVariation)(
        (rep, combinationIterator) => combinationIterator.foldLeft(rep)(
          (repL, ls) => {
            if (request.towerActuators.contains("Pump")) {
              repL.addCombinator(VineSensorList(("Valve", Seq("Relay")) +: ls))
            }
            else {
              repL.addCombinator(VineSensorList(ls))
            }
          }
        )
      )

      val honeycombCombinations = 1.to(honeycombSensorCombDescription.length).map(
        i => honeycombSensorCombDescription.combinations(i))
      val refRepWithHoneycomb = honeycombCombinations.foldLeft(refRepWithVine)(
        (rep, combinationIterator) => combinationIterator.foldLeft(rep)(
          (repL, ls) => repL.addCombinator(HoneyCombSensorList(ls))
        )
      )


      val turbineCombinations = 1.to(turbineSensorDescription.length).map(
        i => turbineSensorDescription.combinations(i))
      val refRepWithTurbine = turbineCombinations.foldLeft(refRepWithHoneycomb)(
        (rep, combinationIterator) => combinationIterator.foldLeft(rep)(
          (repL, ls) => repL.addCombinator(TurbineSensorList(ls))
        )
      )

      val simpleTopCombinations = 1.to(simpleTopSensorDescription.length).map(
        i => simpleTopSensorDescription.combinations(i))
      val refRepWithTop = simpleTopCombinations.foldLeft(refRepWithTurbine)(
        (rep, combinationIterator) => combinationIterator.foldLeft(rep)(
          (repL, ls) => repL.addCombinator(SimpleTopSensorList(ls))
        )
      )

      //Add actuators
      val refRepWithLamp = if (request.towerActuators.contains("Lamp")) {
        refRepWithTop.addCombinator(SimpleTopWithLamp)
      }
      else {
        refRepWithTop
      }
      val refRepWithActuators =
        if (request.towerActuators.contains("Pump")) {
          val refRepWithPump = refRepWithLamp.addCombinator(StackbaseWithPump)
          (request.turbineSensors.contains("Water"),
            request.honeycombSensors.contains("Water")) match {
            case (false, false) => refRepWithPump
            case (false, true) => refRepWithPump
              .addCombinator(HoneyCombLayerArgumentList(Seq(("AnalogueWater", Seq("Water"))), Seq(("Valve", Seq("Relay")))))
            case (true, false) => refRepWithPump
              .addCombinator(TurbineLayerArgumentList(Seq(("AnalogueWater", Seq("Water"))), Seq(("Valve", Seq("Relay")))))
            case (true, true) => refRepWithPump
              .addCombinator(TurbineLayerArgumentList(Seq(("AnalogueWater", Seq("Water"))), Seq(("Valve", Seq("Relay")))))
              .addCombinator(HoneyCombLayerArgumentList(Seq(("AnalogueWater", Seq("Water"))), Seq(("Valve", Seq("Relay")))))
          }
        }
        else {
          val refRep = refRepWithLamp.addCombinator(Stackbase).addCombinator(EmptyLayerArgumentsList)
          (request.turbineSensors.contains("Water"),
            request.honeycombSensors.contains("Water")) match {
            case (false, false) => refRep
            case (false, true) => refRep
              .addCombinator(HoneyCombLayerArgumentList(Seq(("AnalogueWater", Seq("Water"))), Seq()))
            case (true, false) => refRep
              .addCombinator(TurbineLayerArgumentList(Seq(("AnalogueWater", Seq("Water"))), Seq()))
            case (true, true) => refRep
              .addCombinator(TurbineLayerArgumentList(Seq(("AnalogueWater", Seq("Water"))), Seq()))
              .addCombinator(HoneyCombLayerArgumentList(Seq(("AnalogueWater", Seq("Water"))), Seq()))
          }
        }
      refRepWithActuators
    }


    //noinspection AccessorLikeMethodIsEmptyParen
    def toReflectedRepository(): ReflectedRepository[Repository] = {

      val reflectedRep = ReflectedRepository(this, TaxonomyKinding.semanticTaxonomy, TaxonomyKinding.basicKinding) //, algorithm = algo, classLoader = cl)

      addCombinatorsForRequest(reflectedRep)
    }

    @combinator object LayerList0 {
      val semanticType: Type = vec(0, alpha)

      def apply(): Seq[Algebra] = Nil
    }

    @combinator object LayerList1 {
      val semanticType: Type = alpha =>: vec(1, alpha)

      def apply(layer1: Algebra): Seq[Algebra] = Seq(layer1)
    }

    @combinator object LayerList2 {
      val semanticType: Type = alpha =>: alpha =>: vec(2, alpha)

      def apply(layer1: Algebra, layer2: Algebra): Seq[Algebra] = Seq(layer1, layer2)
    }

    @combinator object LayerList3 {
      val semanticType: Type = alpha =>: alpha =>: alpha =>: vec(3, alpha)

      def apply(layer1: Algebra, layer2: Algebra, layer3: Algebra): Seq[Algebra] = Seq(layer1, layer2, layer3)
    }

    @combinator object LayerList4 {
      val semanticType: Type = alpha =>: alpha =>: alpha =>: alpha =>: vec(4, alpha)

      def apply(layer1: Algebra, layer2: Algebra, layer3: Algebra, layer4: Algebra): Seq[Algebra] =
        Seq(layer1, layer2, layer3, layer4)
    }

    @combinator object LayerList5 {
      val semanticType: Type = alpha =>: alpha =>: alpha =>: alpha =>: alpha =>: vec(5, alpha)

      def apply(layer1: Algebra, layer2: Algebra, layer3: Algebra, layer4: Algebra, layer5: Algebra): Seq[Algebra] =
        Seq(layer1, layer2, layer3, layer4, layer5)
    }

    @combinator object Environment extends EnvironmentSig {
      override def apply(rows: Seq[Algebra], sensors: Seq[Algebra]): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.environment(rows.map(row => row.result(algI)), sensors.map(sensor => sensor.result(algI)))
      }
    }

    @combinator object Row extends RowSig {
      override def apply(towers: Seq[Algebra]): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.row(towers.map(tower => tower.result(algI)))
      }
    }

    @combinator object Tower extends TowerSig {
      override def apply(base: Algebra, body: Seq[Algebra], top: Algebra): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.tower(base.result(algI), body.map(layer => layer.result(algI)), top.result(algI))
      }
    }

    /*
        //Überflüssig??!
        @combinator object Staple extends StapleSig {
          override def apply(combination: Seq[Algebra]): Seq[Algebra] = combination
        }
     */

    @combinator object SimpleTop extends SimpleTopSig {
      override def apply(sensors: Seq[Algebra]): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.simpletop(sensors.map(sensor => sensor.result(algI)))
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
        override def result(algI: AlgebraInstance): algI.A =
          algI.honeycombtray(sensors.map(sensor => sensor.result(algI)))
      }
    }

    @combinator object TurbineTray extends TurbineTraySig {
      override def apply(sensors: Seq[Algebra]): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.turbinetray(sensors.map(sensor => sensor.result(algI)))
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

    @combinator object EmptyActuatorList extends FixedActuatorListSig {
      override def apply(): Seq[Algebra] = Nil
    }

    @combinator object EmptySensorAndActuatorList extends FixedSensorAndActuatorListSig {
      override def apply(): Seq[Algebra] = Nil
    }

    @combinator object DHT22Sensor extends DHT22SensorSig {
      override def apply(): Algebra = new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.sensor("Dht22", Seq("Temperature", "Humidity"))
      }
    }

  }

  //Variation
  /*
  Eine Variation nimmt einen Int length für ihre Länge und einen Int index für den Index des length-Tuples, das der Variation
  von Layern für den TowerBody entspricht.
   */

  object HoneyCombLayer extends HoneyCombLayerSig {
    //override def apply(tray1: Algebra, tray2: Algebra, tray3: Algebra): Algebra = new Algebra {
    override def apply(tray: Algebra, sensors: Seq[Algebra]): Algebra = new Algebra {
      override def result(algI: AlgebraInstance): algI.A = {
        val trays = Seq(tray, tray, tray)
        algI.honeycomblayer(trays.map(tray => tray.result(algI)), sensors.map(sensor => sensor.result(algI)))
      }
    }
  }

  object EmptyLayerArgumentsList extends FixedLayerArgumentsListSig {
    override def apply(): Seq[Algebra] = Nil
  }

  object Stackbase extends StackbaseSig {
    override def apply(): Algebra = new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.stackbase(Seq())
    }
  }

  sealed case class TurbineLayerArgumentList(sensors: Seq[(String, Seq[String])], actuators: Seq[(String, Seq[String])]) {
    val semanticType: Type = list(sensorAndActuator) :&: turbine :&: layer

    def apply(): Seq[Algebra] = sensors.map(sensorDescription => new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.sensor(sensorDescription._1, sensorDescription._2)
    }) ++ actuators.map(actuatorDescription => new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.actuator(actuatorDescription._1, actuatorDescription._2)
    })
  }

  sealed case class HoneyCombLayerArgumentList(sensors: Seq[(String, Seq[String])], actuators: Seq[(String, Seq[String])]) {
    val semanticType: Type = list(sensorAndActuator) :&: honeyComb :&: layer

    def apply(): Seq[Algebra] = sensors.map(sensorDescription => new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.sensor(sensorDescription._1, sensorDescription._2)
    }) ++ actuators.map(actuatorDescription => new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.actuator(actuatorDescription._1, actuatorDescription._2)
    })
  }

  object StackbaseWithPump extends StackbaseSig {
    override def apply(): Algebra = new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.stackbase(Seq(algI.actuator("Pump", Seq("Relay"))))
    }
  }

  object TurbineLayer extends TurbineLayerSig {
    //override def apply(tray1: Algebra, tray2: Algebra, tray3: Algebra): Algebra = new Algebra {
    override def apply(tray: Algebra, sensors: Seq[Algebra]): Algebra = new Algebra {
      override def result(algI: AlgebraInstance): algI.A = {
        val trays = Seq(tray, tray, tray)
        algI.turbinelayer(trays.map(tray => tray.result(algI)), sensors.map(sensor => sensor.result(algI)))
      }
    }
  }

  object VineLayer extends VineLayerSig {
    override def apply(sensors: Seq[Algebra]): Algebra = new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.vinelayer(sensors.map(sensor => sensor.result(algI)))
    }
  }

  object DoubleVineLayer extends DoubleVineLayerSig {
    override def apply(sensors: Seq[Algebra]): Algebra = new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.doublevinelayer(sensors.map(sensor => sensor.result(algI)))
    }
  }

  object DHT22SensorList extends FixedSensorListSig {
    override def apply(): Seq[Algebra] =
      Seq(new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.sensor("Dht22", Seq("Temperature", "Humidity"))
      })
  }

  object DHT22VineSensorList extends VineSensorListSig {
    override def apply(): Seq[Algebra] =
      Seq(new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.sensor("Dht22", Seq("Temperature", "Humidity"))
      })
  }

  object DHT22TurbineSensorList extends TurbineSensorListSig {
    override def apply(): Seq[Algebra] =
      Seq(new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.sensor("Dht22", Seq("Temperature", "Humidity"))
      })
  }

  object DHT22HoneyCombSensorList extends HoneyCombSensorListSig {
    override def apply(): Seq[Algebra] =
      Seq(new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.sensor("Dht22", Seq("Temperature", "Humidity"))
      })
  }

  object SimpleTopWithLamp extends SimpleTopSig {
    override def apply(sensors: Seq[Algebra]): Algebra = new Algebra {
      override def result(algI: AlgebraInstance): algI.A =
        algI.simpletop(algI.actuator("Led", Seq("Relay")) +: sensors.map(sensor => sensor.result(algI)))
    }
  }

  sealed case class VineSensorList(sensors: Seq[(String, Seq[String])]) extends VineSensorListSig {
    override def apply(): Seq[Algebra] =
      sensors.map(sensorDescription => new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.sensor(sensorDescription._1, sensorDescription._2)
      })
  }

  sealed case class SimpleTopSensorList(sensors: Seq[(String, Seq[String])]) extends SimpleTopSensorListSig {
    override def apply(): Seq[Algebra] =
      sensors.map(sensorDescription => new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.sensor(sensorDescription._1, sensorDescription._2)
      })
  }

  sealed case class TurbineSensorList(sensors: Seq[(String, Seq[String])]) extends TurbineSensorListSig {
    override def apply(): Seq[Algebra] =
      sensors.map(sensorDescription => new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.sensor(sensorDescription._1, sensorDescription._2)
      })
  }

  sealed case class HoneyCombSensorList(sensors: Seq[(String, Seq[String])]) extends HoneyCombSensorListSig {
    override def apply(): Seq[Algebra] =
      sensors.map(sensorDescription => new Algebra {
        override def result(algI: AlgebraInstance): algI.A =
          algI.sensor(sensorDescription._1, sensorDescription._2)
      })
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

    //case class DoubleVine() extends LayerVTH with LayerVT with LayerVH

  }

  object Variation0 {
    val semanticType: Type = towerCombination

    def apply(): Seq[Algebra] = Nil
  }

}

