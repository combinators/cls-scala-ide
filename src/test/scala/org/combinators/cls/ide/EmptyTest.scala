package org.combinators.cls.ide

import java.nio.file.Path
import javax.inject.Inject

import controllers.Assets
import org.combinators.cls.types._
import org.scalatestplus.play._
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.WSClient
import play.api.test.Helpers._
import org.combinators.cls.ide.Helpers._
import org.combinators.cls.git.EmptyResults
import org.combinators.templating.persistable.Persistable


class EmptyTest extends PlaySpec with GuiceOneServerPerSuite {

  val index: Int = 100
  val client = app.injector.instanceOf[WSClient]
  "Calling the emptyTest index" must {
    "result in a valid response" in {
      val request = s"/testPrefix/emptytest/ide"
      val url = s"http://localhost:$port$request"
      val response = await(client.url(url).get())
      response.status mustBe OK
    }
  }
  "Calling the test graphEmpty" must {
    "result in a valid response" in {
      val request = s"/testPrefix/emptytest/graph"
      val url = s"http://localhost:$port$request"
      val response = await(client.url(url).get())
      response.status mustBe OK
    }
  }
  "Calling the test showResultEmpty" must {
    "result in a valid response" in {
        val request = s"/testPrefix/emptytest/showResult/$index"
        val url = s"http://localhost:$port$request"
        val response = await(client.url(url).get())
        response.status mustBe NOT_FOUND
      }
      }
  "Calling the test showOnePossibleSolutionGraph" must {
    "result in a valid response" in {
        val request = s"/testPrefix/emptytest/showOnePossibleSolutionGraph/$index"
        val url = s"http://localhost:$port$request"
        val response = await(client.url(url).get())
        response.status mustBe NOT_FOUND

      }
  }
  "Calling the test computeRequestEmpty" must {
    "result in a valid response" in {
      val request = s"/testDebugger/computeRequest/test"
      val url = s"http://localhost:$port$request"
      val response = await(client.url(url).get())
      response.status mustBe OK
      response.body.toLowerCase.indexOf("not found!") must be > 0
    }
  }
  }


class EmptyTestController @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle, assets: Assets, example: TestRepository, exampleName: String)
  extends Debugger(webJars,
    assets,
    Kinding.empty,
    example.GammaFin.repository,
    example.GammaFin.subtypes,
    Seq(Constructor("Impossible")),
    example.resultsInhabit1.infinite,
    EmptyResults().add(example.results),
    example.testChannel,
    "test") with RoutingEntries {
  override val controllerAddress: String = "emptytest"
  override val routingPrefix: Option[String] = Some("testPrefix")
  implicit val persistable: Persistable.Aux[Path] = new Persistable {
    override type T = Path
    override def rawText(elem: T): Array[Byte] = elem.toString.getBytes
    override def path(elem: T): Path = elem
  }
}





