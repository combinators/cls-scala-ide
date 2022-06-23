package org.combinators.cls.ide.examples.hydrocluster

import org.combinators.cls.types.{Kinding, NonEmptyTaxonomy, Taxonomy}
import org.combinators.cls.ide.examples.hydrocluster.SemanticTypes._


object TaxonomyKinding {

  lazy val taxonomy: List[NonEmptyTaxonomy] =
  Taxonomy(term.name)
    .addSubtype(environment.name)
    .addSubtype(row.name)
    .addSubtype(base.name)
    .addSubtype(top.name)
    .addSubtype(sensor.name)
    .addSubtype(tray.name)
    .addSubtype(layer.name)
    .addSubtype(tower.name) ::
  Taxonomy(sensorType.name)
    .addSubtype(temperature.name)
    .addSubtype(humidity.name)
    .addSubtype(uvLevel.name)
    .addSubtype(sealevel.name)
    .addSubtype(water.name)
    .addSubtype(moisture.name)
    .addSubtype(pressure.name)
    .addSubtype(altitude.name) ::
  Taxonomy(sensorName.name)
    .addSubtype(dht22.name)
    .addSubtype(bmp280.name)
    .addSubtype(bmp085.name)
    .addSubtype(veml6070.name)
    .addSubtype(mcp3008.name) ::
  Taxonomy(sensor.name)
    .addSubtype(digital.name) ::
  Nil
  /*
  Taxonomy(layer.name)
    .addSubtype(honeyCombLayer.name)
    .addSubtype(turbineLayer.name)
    .addSubtype(vineLayer.name) ::
  Taxonomy(tray.name)
    .addSubtype(honeyCombTray.name)
    .addSubtype(turbineTray.name) ::
  Nil

   */

  lazy val semanticTaxonomy: Taxonomy = taxonomy.foldLeft(Taxonomy.empty)((a, b) => a.merge(b))

  lazy val kindingList: List[Kinding] =
  Kinding(alpha)
    .addOption(honeyCombLayer)
    .addOption(turbineLayer)
    .addOption(vineLayer)
    .addOption(doubleVineLayer) ::
  Kinding(beta)
    .addOption(vine)
    .addOption(turbine)
    .addOption(honeyComb)
    .addOption(doubleVine)::
  Kinding(gamma)
    .addOption(sensor)
    .addOption(actuator)
    .addOption(sensorAndActuator)::
  Nil

  lazy val basicKinding: Kinding = kindingList.foldLeft(Kinding.empty)((a, b) => a.merge(b))

}
