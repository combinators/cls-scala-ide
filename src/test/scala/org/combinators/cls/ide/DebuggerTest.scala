package org.combinators.cls.ide
import javax.inject.Inject

import controllers.Assets
import org.combinators.cls.git.EmptyResults
import org.combinators.cls.types.{Kinding, SubtypeEnvironment}
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.WSClient
import play.api.test.Helpers._
import org.combinators.cls.ide.Helpers._


class DebuggerTest extends PlaySpec with GuiceOneServerPerSuite {
  val ws = app.injector.instanceOf[WSClient]
  "Calling the debuggerTest index" must {
    "result in a valid response" in {
      val request = s"/testDebugger/ide"
      val url = s"http://localhost:$port$request"
      val testPaymentGatewayURL = s"http://$url"
      val response = await(ws.url(url).get())
      response.status mustBe OK
      response.body.toLowerCase.indexOf("repository") must be > 0
    }
  }
  "Calling the test showGraph" must {
    "result in a valid response" in {
      val request = s"/test/graph"
      val url = s"http://localhost:$port$request"
      val testPaymentGatewayURL = s"http://$url"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }
}

class DebuggerTestController @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle, assets: Assets, example: TestRepository, exampleName: String)
  extends Debugger(
    webJars,
    assets,
    Kinding.empty,
    example.GammaFin.repository,
    example.GammaFin.subtypes,
    Seq(example.target),
    example.resultsIntabit1.infinite,
    EmptyResults().add(example.results),
    example.testChannel,
    "test") with RoutingEntries {
  override val controllerAddress: String = "testDebugger"
}
