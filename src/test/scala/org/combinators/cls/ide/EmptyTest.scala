package org.combinators.cls.ide

import javax.inject.Inject

import controllers.Assets
import org.combinators.cls.types._
import org.scalatestplus.play._
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.WSClient
import play.api.test.Helpers._


class EmptyTest extends PlaySpec with GuiceOneServerPerSuite {
  val ws = app.injector.instanceOf[WSClient]
  "Calling the emptytest index" must {
      "result in a valid response" in {
        val request = s"/test/ide"
        val url = s"http://localhost:$port$request"
        val testPaymentGatewayURL = s"http://$url"
        val response = await(ws.url(url).get())
        response.status mustBe OK
        response.body.toLowerCase.indexOf("repository") must be > 0
      }
    }

  "Calling the test graph" must {
    "result in a valid response" in {
      val request = s"/test/graph"
      val url = s"http://localhost:$port$request"
      val testPaymentGatewayURL = s"http://$url"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }

  "Calling the test repository" must {
    "result in a valid response" in {
      val request = s"/test/repository"
      val url = s"http://localhost:$port$request"
      val testPaymentGatewayURL = s"http://$url"
      val response = await(ws.url(url).get())
      response.status mustBe OK
      response.body.toLowerCase.indexOf("repository") must be > 0
    }
  }
  "Calling the test showDebuggerMessages" must {
    "result in a valid response" in {
      val request = s"/test/showDebuggerMessages"
      val url = s"http://localhost:$port$request"
      val testPaymentGatewayURL = s"http://$url"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }

  "Calling the test showUninhabitedTy" must {
    "result in a valid response" in {
      val request = s"/test/showUninhabitedTy"
      val url = s"http://localhost:$port$request"
      val testPaymentGatewayURL = s"http://$url"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }

  "Calling the test showUnusableCMsg" must {
    "result in a valid response" in {
      val request = s"/test/showUnusableCMsg"
      val url = s"http://localhost:$port$request"
      val testPaymentGatewayURL = s"http://$url"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }

  "Calling the test showUnusableBecauseOfTy" must {
    "result in a valid response" in {
      val request = s"/test/showUnusableBecauseOfTy"
      val url = s"http://localhost:$port$request"
      val testPaymentGatewayURL = s"http://$url"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }


  "Calling the test steps" must {
    "result in a valid response" in {
      val request = s"/test/steps/1"
      val url = s"http://localhost:$port$request"
      val testPaymentGatewayURL = s"http://$url"
      val response = await(ws.url(url).get())
      response.status mustBe OK

    }
  }

  "Calling the test computeRequest" must {
    "result in a valid response" in {
      val request = s"/test/computeRequest/test"
      val url = s"http://localhost:$port$request"
      val testPaymentGatewayURL = s"http://$url"
      val response = await(ws.url(url).get())
      response.status mustBe OK
    }
  }
  "Calling the test toggleCycles" must {
    "result in a valid response" in {
      for(resultNumber <- Expected.expectedPaths.toSeq.indices) {
        val request = s"/test/toggleCycle/$resultNumber"
        val url = s"http://localhost:$port$request"
        val testPaymentGatewayURL = s"http://$url"
        val response = await(ws.url(url).get())
        response.status mustBe OK
      }
    }
  }

  "Calling the test showPosition" must {
    "result in a valid response" in {
      val request = s"/test/showPosition/Garbage2"
      val url = s"http://localhost:$port$request"
      val testPaymentGatewayURL = s"http://$url"
      val response = await(ws.url(url).get())
      response.status mustBe OK

    }
  }
  }


class EmptyTestController @Inject()(webJars: WebJarsUtil, applicationLifecycle: ApplicationLifecycle, assets: Assets, example: TestRepository, exampleName: String)
  extends Debugger(webJars,
    assets,
    example.Gamma.substitutionSpace,
    SubtypeEnvironment(example.Gamma.nativeTypeTaxonomy.addNativeType[Unit].taxonomy.merge(example.Gamma.semanticTaxonomy).underlyingMap),
    example.jobs.targets,
    example.resultsIntabit.infinite,
    example.Gamma.combinatorComponents,
    example.resultsIntabit,
    example.testChannel,
    "test") with RoutingEntries {
  override val controllerAddress: String = "test"
}





