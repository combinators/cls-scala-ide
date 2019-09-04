package org.combinators.cls.ide.examples.flocking

import org.combinators.cls.interpreter._
import org.combinators.cls.types.{Constructor, Kinding, Type, Variable}
import org.combinators.cls.types.syntax._

import scala.io.Source

trait FlockingRepository {

  val pursuit_var = Variable("pursuit_var")
  val pursuit = Constructor("pursuit")
  val mlPursuit = Constructor("mlPursuit")

  val wandering_var = Variable("wandering_var")
  val wandering = Constructor("wandering")

  val cohesion_var = Variable("cohesion_var")
  val cohesion = Constructor("cohesion")

  val alignment_var = Variable("alignment_var")
  val alignment = Constructor("alignment")

  val separation_var = Variable("separation_var")
  val separation = Constructor("separation")

  val leaderController = Constructor("leaderController")
  val leaderControllerFile = Constructor("leaderControllerFile")

  def buildKinding(m: Map[Variable, Seq[Type]]): Kinding =
    (m map (a => a._2.foldLeft(Kinding(a._1))((a, b) => a.addOption(b)))).reduce((k1, k2) => k1.merge(k2))

  val map = Map(
    pursuit_var -> Seq(pursuit, mlPursuit),
    wandering_var -> Seq(wandering),
    cohesion_var -> Seq(cohesion),
    alignment_var -> Seq(alignment),
    separation_var -> Seq(separation))

  val kinding: Kinding = buildKinding(map)

  @combinator object LeaderControllerCombinator {
    def apply(pursuitBehaviour: String, wanderingBehaviour: String, cohesionBehaviour: String,
              alignmentBehaviour: String, separationBehaviour: String): String =
      Source.fromResource("LeaderControllerTemplate.code").getLines.mkString("\n").
        format(pursuitBehaviour, wanderingBehaviour, cohesionBehaviour, alignmentBehaviour, separationBehaviour)

    val semanticType = pursuit_var =>: wandering_var =>: cohesion_var =>: alignment_var =>:
      separation_var =>: leaderController
  }

  @combinator object LeaderControllerToFileCombinator {
    def apply(leaderController: String): Unit =
      reflect.io.File("src\\main\\resources\\out.cs").writeAll(leaderController)

    val semanticType = leaderController =>: leaderControllerFile
  }

  @combinator object PursuitBehaviorCombinator {
    def apply: String = "nextRotation = Quaternion.LookRotation(pursuitBehavior.GetPursuitVector().Value, Vector3.up);"

    val semanticType = pursuit
  }

  @combinator object MlPursuitBehaviourCombinator {
    def apply: String = "pursuitInferenceAgent.position = transform.position;\n" +
      "pursuitInferenceAgent.target = pursuitBehavior.target.Value;\n" +
      "pursuitInferenceAgent.RequestDecision();\n" +
      "var vector = pursuitInferenceAgent.output;\n" +
      "vector.y = pursuitBehavior.GetPursuitVector().Value.y;\n" +
      "nextRotation = Quaternion.LookRotation(vector, Vector3.up);"

    val semanticType = mlPursuit
  }

  @combinator object WanderingCombinator {
    def apply: String = "wanderingBehavior.Update(Time.deltaTime);\n " +
      "nextRotation = Quaternion.Slerp(transform.rotation, wanderingBehavior.targetRotation, " +
      "Time.deltaTime * wanderingBehavior.directionChangeInterval);"

    val semanticType = wandering
  }

  @combinator object CohesionCombinator {
    def apply: String = "nextRotation = Quaternion.Slerp(nextRotation," +
      "Quaternion.LookRotation(cohesionVector), 0.015f); //0.1f"

    val semanticType = cohesion
  }

  @combinator object AlignmentCombinator {
    def apply: String = "nextRotation = Quaternion.Slerp(nextRotation, " +
      "Quaternion.LookRotation(alignmentVector), 0.1f); //0.2f"

    val semanticType = alignment
  }

  @combinator object SeparationCombinator {
    def apply: String = "nextRotation = Quaternion.Slerp(nextRotation, Quaternion.LookRotation(separationVector), 1f);"

    val semanticType = separation
  }

}
