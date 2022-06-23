package org.combinators.cls.ide

import java.nio.file.Path
import javax.inject.Inject
import controllers.Assets
import org.combinators.cls.inhabitation.BoundedCombinatoryLogic
import org.combinators.cls.interpreter.ReflectedRepository
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import org.webjars.play.WebJarsUtil
import play.api.libs.ws.WSClient
import play.api.test.Helpers._
import org.combinators.cls.types.{Constructor, FiniteSubstitutionSpace, Type}
import org.combinators.templating.persistable.Persistable
import play.api.mvc.RequestHeader
import play.api._
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc._
import play.api.mvc.Results._
import play.api.test.FakeRequest



class DebuggerTest extends PlaySpec with GuiceOneServerPerSuite {
  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .appRoutes(app => {
        case ("GET", "/") => app.injector.instanceOf(classOf[DefaultActionBuilder]) {
          Ok("ok")
        }
      })
      .build()
  }

  //"test server logic" in {
  //    val wsClient              = app.injector.instanceOf[WSClient]
  //    val myPublicAddress       = s"localhost:$port"
  //    val testPaymentGatewayURL = s"http://$myPublicAddress"
  //    // The test payment gateway requires a callback to this server before it returns a result...
  //    val callbackURL = s"http://$myPublicAddress/callback"
  //    // await is from play.api.test.FutureAwaits
  //    val response =
  //      await(wsClient.url(testPaymentGatewayURL).addQueryStringParameters("callbackURL" -> callbackURL).get())
  //
  //    response.status mustBe OK
  //  }

  val ws = app.injector.instanceOf[WSClient]

 "Calling the debuggerTest index" must {
   "result in a valid response" in {
     val request = s"/testDebugger/ide"
     val url = s"http://localhost:$port$request"
     val callback = s"http://localhost:$port"
     // The test payment gateway requires a callback to this server before it returns a result...
         val callbackURL = s"http://$url/callback"
     val response = await(ws.url(url).addQueryStringParameters("callbackURL" -> callbackURL).get())
     response.status mustBe OK
   }
 }

/* "Calling the debuggerTest ide" must {
  "result in a valid response" in {
    val request = s"/testDebugger/ide"
    val url = s"http://localhost:$port$request"
    val response = await(ws.url(url).get())
    response.status mustBe OK
    //response.body.toLowerCase.indexOf("repository") must be > 0
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

"Calling the test smt" must {
  "result in a valid response" in {
    val request = s"/testDebugger/smt"
    val url = s"http://localhost:$port$request"
    val response = await(ws.url(url).get())
    response.status mustBe OK
  }
}

"Calling the test show organized types" must {
  "result in a valid response" in {
    val request = s"/testDebugger/showOrganizedTy"
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


}
*/
}
class DebuggerTestController @Inject()(val webJarsUtil: WebJarsUtil, assets: Assets)
extends DebuggerController(webJarsUtil, assets) with DebuggerEnabled {
  override val controllerAddress: String = "testDebugger"
  lazy val target: Type = example.target
  lazy val example = new TestRepository

  lazy val GammaBcl = example.GammaBCL
  //tgts = Seq(target)
  lazy val result2 = GammaBcl.inhabit(target)
  bcl = Some(example.GammaBCL)
  //override lazy val result = example.results
  override val projectName: String = controllerAddress
  override val tgts: Seq[Type] = Seq(target)
  override val refRepo: Option[ReflectedRepository[_]] = Some(Gamma)
  override val reposit: Option[Map[String, Type]] = Some(example.garbageCombinators)

  lazy val Gamma = ReflectedRepository(example.garbageCombinators,
    substitutionSpace = FiniteSubstitutionSpace.empty,
    semanticTaxonomy = example.taxonomy,
    classLoader = this.getClass.getClassLoader,
    algorithm = debugger())
  //refRepository = Some(example.Gamma)
  //debugger.computeResults(Gamma, Seq(target), Some(example.garbageCombinators))
  //debugger.mkModel
  implicit val persistable: Persistable.Aux[Path] = new Persistable {
    override type T = Path

    override def rawText(elem: T): Array[Byte] = elem.toString.getBytes

    override def path(elem: T): Path = elem
  }
}

