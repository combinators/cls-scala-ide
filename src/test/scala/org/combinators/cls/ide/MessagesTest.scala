package org.combinators.cls.ide

import java.nio.file.Path
import javax.inject.Inject

import controllers.Assets
import org.combinators.cls.types._
import org.combinators.templating.persistable.Persistable
import org.scalatestplus.play._
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.WSClient
import play.api.test.Helpers._



class MessagesTest extends PlaySpec with GuiceOneServerPerSuite {
    val ws = app.injector.instanceOf[WSClient]

  "Calling the test prefixTest" must {
    "result in a valid response" in {
      val request = s"/testMessages/ide/"
      val url = s"http://localhost:$port$request"
      val response = await(ws.url(url).get())
      response.status mustBe OK

    }
  }

    "Calling the test showUninhabitedTy" must {
        "result in a valid response" in {
            val request = s"/testMessages/showUninhabitedTy"
            val url = s"http://localhost:$port$request"
            val response = await(ws.url(url).get())
            response.status mustBe OK

        }
    }

    "Calling the test showDebuggerMessages" must {
        "result in a valid response" in {
            val request = s"/testMessages/showDebuggerMessages"
            val url = s"http://localhost:$port$request"
            val response = await(ws.url(url).get())
            response.status mustBe OK
            response.body.toLowerCase.indexOf(" ") must be > 0
        }
    }
}

class MessageTestController @Inject()(val webJarsUtil: WebJarsUtil, assets: Assets)
  extends DebuggerController(
    webJarsUtil,
      assets) with DebuggerEnabled {
  override val controllerAddress: String = "testMessages"
 // override val routingPrefix: Option[String] = Some("/testMessages")
  implicit val persistable: Persistable.Aux[Path] = new Persistable {
    override type T = Path
    override def rawText(elem: T): Array[Byte] = elem.toString.getBytes
    override def path(elem: T): Path = elem
  }
}
