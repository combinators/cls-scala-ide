package org.combinators.cls.ide.examples.hydrocluster

object Algebraic {

  @SerialVersionUID(100L)
  trait Algebra extends Serializable {
    def result(algI: AlgebraInstance): algI.A
  }

  trait AlgebraInstance {
    type A

    def environment(rows: Seq[A], sensors: Seq[A]): A

    def row(towers: Seq[A]): A

    def tower(base: A, layers: Seq[A], top: A): A

    def stackbase(sensors: Seq[A]): A

    def simpletop(sensors: Seq[A]): A

    def honeycomblayer(trays: Seq[A], sensors: Seq[A]): A

    def turbinelayer(trays: Seq[A], sensors: Seq[A]): A

    def vinelayer(sensors: Seq[A]): A

    def doublevinelayer(sensors: Seq[A]): A

    def honeycombtray(sensors: Seq[A]): A

    def turbinetray(sensors: Seq[A]): A

    def sensor(sensorName: String, sensortypes: Seq[String]): A

    def actuator(actuatorName: String, actuatorTypes: Seq[String]): A
  }

}
