package org.combinators.cls.ide
import javax.inject.Inject

import controllers.Assets
import org.combinators.cls.git.EmptyResults
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.WSClient
import play.api.test.Helpers._
import Helpers._
import org.combinators.cls.types.Constructor


class DebuggerTest extends PlaySpec with GuiceOneServerPerSuite {
  val ws = app.injector.instanceOf[WSClient]
  "Calling the debuggerTest index" must {
    "result in a valid response" in {
      val request = s"/testDebugger"
      val url = s"http://localhost:$port$request"
      val response = await(ws.url(url).get())
      response.status mustBe OK
      response.body.toLowerCase.indexOf("repository") must be > 0
    }
  }

  "Calling the test graph" must {
    "result in a valid response" in {
      val request = s"/testDebugger/graph"
      val url = s"http://localhost:$port$request"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }

  "Calling the test repository" must {
    "result in a valid response" in {
      val request = s"/testDebugger/repository"
      val url = s"http://localhost:$port$request"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }
  "Calling the test showDebuggerMessages" must {
    "result in a valid response" in {
      val request = s"/testDebugger/showDebuggerMessages"
      val url = s"http://localhost:$port$request"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }

  "Calling the test showUninhabitedTy" must {
    "result in a valid response" in {
      val request = s"/testDebugger/showUninhabitedTy"
      val url = s"http://localhost:$port$request"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }

  "Calling the test showUnusableCMsg" must {
    "result in a valid response" in {
      val request = s"/testDebugger/showUnusableCMsg"
      val url = s"http://localhost:$port$request"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }

  "Calling the test showUnusableBecauseOfTy" must {
    "result in a valid response" in {
      val request = s"/testDebugger/showUnusableBecauseOfTy"
      val url = s"http://localhost:$port$request"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }


  "Calling the test steps" must {
    "result in a valid response" in {
      val size: Int = 10
      for (stepNumber <- 1 to size) {
        val request = s"/testDebugger/steps/$stepNumber"
        val url = s"http://localhost:$port$request"
        val response = await(ws.url(url).get())
        response.status mustBe OK
      }
    }
  }
  "Calling the test computeRequest" must {
    "result in a valid response" in {
      val newTarget = Constructor("Int")
      val request = s"/testDebugger/computeRequest/$newTarget"
      val url = s"http://localhost:$port$request"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }

  "Calling the test toggleCycles" must {
    "result in a valid response" in {
      val request = s"/testDebugger/toggleCycle/1"
      val url = s"http://localhost:$port$request"
      val response = await(ws.url(url).get())
      response.status mustBe OK
      response.body.toLowerCase.indexOf("nodes") must be > 0
    }
  }
  "Calling the test showResult" must {
    "result in a valid response" in {
      val testRepository = new TestRepository
      for(resultIndex <- testRepository.inhabitantsNumber.indices) {
        val request = s"/testDebugger/showResult/$resultIndex"
        val url = s"http://localhost:$port$request"
        val response = await(ws.url(url).get())
        response.status mustBe OK
      }
    }
  }
  "Calling the test showOnePossibleSolutionGraph" must {
    "result in a valid response" in {
      val testRepository = new TestRepository
      for(resultIndex <- testRepository.inhabitantsNumber.indices) {
        val request = s"/testDebugger/showOnePossibleSolutionGraph/$resultIndex"
        val url = s"http://localhost:$port$request"
        val response = await(ws.url(url).get())
        response.status mustBe OK
        response.body.toLowerCase.indexOf("nodes") must be > 0
      }
    }
  }
  "Calling the test countSolutions" must {
    "result in a valid response" in {
      val request = s"/testDebugger/countSolutions"
      val url = s"http://localhost:$port$request"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }
  "Calling the test showPosition" must {
    "result in a valid response" in {
      val request = s"/testDebugger/showPosition/f"
      val url = s"http://localhost:$port$request"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }

}

class DebuggerTestController @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle, assets: Assets, example: TestRepository, exampleName: String)
  extends Debugger(
    webJars,
    assets,
    example.Gamma.substitutionSpace,
   example.GammaFin.repository,
    example.GammaFin.subtypes,
    Seq(example.target),
    example.results.isInfinite,
    EmptyResults().add(example.results),
    example.testChannel,
    exampleName) with RoutingEntries {
  override val controllerAddress: String = "testDebugger"
}
