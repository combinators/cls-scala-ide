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
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.templating.persistable.Persistable
import play.api.test.FakeRequest


class EmptyTest extends PlaySpec with GuiceOneServerPerSuite {


  "Calling the emptytest overview" must {
    "result in a valid response" in {
      val client = app.injector.instanceOf[WSClient]
      val request = s"/emptytest/ide"
      val url = s"http://localhost:$port$request"
      val response = await(client.url(url).get())
      response.status mustBe OK
      response.body.toLowerCase.indexOf("repository") must be > 0
    }
  }

  val index: String = "1"
  val client = app.injector.instanceOf[WSClient]
  "Calling the emptyTest index" must {
    "result in a valid response" in {
      val request = s"/emptytest/"
      val url = s"http://localhost:$port$request"
      val response = await(client.url(url).get())
      response.status mustBe OK
    }
  }

  "Calling the test graphEmpty" must {
    "result in a valid response" in {
      val request = s"/emptytest/graph"
      val url = s"http://localhost:$port$request"
      val response = await(client.url(url).get())
      response.status mustBe OK
    }
  }
  "Calling the test showDebuggerMessages" must {
    "result in a valid response" in {
      val request = s"/emptytest/showDebuggerMessages"
      val url = s"http://localhost:$port$request"
      val response = await(client.url(url).get())
      response.status mustBe OK
    }
  }

  "Calling the test showResultEmpty" must {
    "result in a valid response" in {
      val request = FakeRequest(GET, s"/emptytest/showResult/1")
      val show = route(app, request).get
      status(show) mustBe NOT_FOUND
      }
      }
  "Calling the test showOnePossibleSolutionGraph" must {
     "result in a valid response" in {
         val request = s"/emptytest/showOnePossibleSolutionGraph/4"
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
  "Calling the test steps" must {
    "result in a invalid response" in {
      val request = FakeRequest(GET, "/testDebugger/steps/3")
      val show = route(app, request).get
      status(show) mustBe OK
    }
  }


  "Calling the test empty Repository" must {
    "result in a valid response" in {
      val request = s"/testDebugger/repository"
      val url = s"http://localhost:$port$request"
      val response = await(client.url(url).get())
      response.status mustBe OK
    //  response.body.toLowerCase.indexOf("not found!") must be > 0
    }
  }



  }

class EmptyTestController @Inject()(val webJarsUtil: WebJarsUtil, assets: Assets)
  extends DebuggerController(webJarsUtil,
    assets) with DebuggerEnabled {
  override val controllerAddress: String = "emptytest"
  override val tgts: Seq[Type] = Seq(target)
  override val refRepo: Option[ReflectedRepository[_]] = Some(Gamma)
  //override val routingPrefix: Option[String] = Some("/emptytest")
  lazy val target: Type = example.targetEmpty
  lazy val example = new TestRepository
  //alternative goals:
  /* lazy val target: Type = (artifact(artifact.impl) :&: precision(precision.integer) :&: unit(unit.celsius))*/

  lazy val Gamma = ReflectedRepository(Map.empty,
    substitutionSpace = FiniteSubstitutionSpace.empty,
    semanticTaxonomy = example.taxonomy,
    classLoader = this.getClass.getClassLoader,
    algorithm = debugger())
  //tgts = Seq(target)
  lazy val result2 = Gamma.inhabit(target)

  /*implicit val persistable: Persistable.Aux[Path] = new Persistable {
    override type T = Path
    override def rawText(elem: T): Array[Byte] = elem.toString.getBytes
    override def path(elem: T): Path = elem
  }*/
}





