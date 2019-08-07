import org.combinators.cls.ide.TestRepository
import org.combinators.cls.types.Constructor
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Play
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._

class DebuggerTestWithLabels extends PlaySpec with BeforeAndAfterAll {
  val app = new GuiceApplicationBuilder().build()


  "Calling the test steps" must {
    "result in a invalid response" in {
      val request = FakeRequest(GET, "/testDebugger/steps/1")
      val show = route(app, request).get
      status(show) mustBe OK
    }
  }
  "Calling the test computeRequest" must {
    "result in a valid response" in {
      //lazy val example = new TestRepository
      val request = FakeRequest(GET, s"/testDebugger/computeRequest/Goal")
      val show = route(app, request).get
      status(show) mustBe OK
    }
  }

  "Calling the test showPosition" must {
    "result in a valid response" in {
      val request = FakeRequest(GET, "/testDebugger/showPosition/x")
      val show = route(app, request).get
      status(show) mustBe OK
    }
  }
  "Calling the test countSolutions" must {
    "result in a valid response" in {
      val request = FakeRequest(GET, "/testDebugger/countSolutions")
      val show = route(app, request).get
      status(show) mustBe OK
    }
  }
  "Calling the test show paths" must {
    "result in a valid response" in {
      val request = FakeRequest(GET, "/testDebugger/showPaths/0")
      val show = route(app, request).get
      status(show) mustBe OK
    }
  }
  "Calling the test compute number of args" must {
    "result in a valid response" in {
      val request = FakeRequest(GET, "/testDebugger/computeNumberOfArgs/x")
      val show = route(app, request).get
      status(show) mustBe OK
    }
  }
  //Todo:
  /*"Calling the test show paths to cover" must {
    "result in a valid response" in {
      val path: String = "(List(),Int->Goal)"
      val request = FakeRequest(GET, s"/testDebugger/showToCover/path")
      val show = route(app, request).get
      status(show) mustBe OK
    }
  }*/

  "Calling the test show paths to cover" must {
    "result in a valid response" in {
      val path: String = "(List(Int),Goal)"
      val request = FakeRequest(GET, s"/testDebugger/showToCover/$path")
      val show = route(app, request).get
      status(show) mustBe OK
    }
  }
  //Todo
  "Calling the test inhabitation without Combinator" must {
    "result in a valid response" in {
      val request = FakeRequest(GET, s"/testDebugger/inhabitantsWithoutCombinator/items?tag=2")
      val show = route(app, request).get
      status(show) mustBe OK
    }
  }

  "Calling the test showOnePossibleSolutionGraph" must {
    "result in a valid response" in {
      val request = FakeRequest(GET, s"/testDebugger/showOnePossibleSolutionGraph/0")
      val show = route(app, request).get
      status(show) mustBe OK
      }
    }
  "Calling the test showResult" must {
    "result in a valid response" in {
        val request = FakeRequest(GET, s"/testDebugger/showResult/0")
        val show = route(app, request).get
        status(show) mustBe OK
    }
  }

   "Calling the test toggleCycles" must {
    "result in a valid response" in {
      val request = FakeRequest(GET, s"/testDebugger/toggleCycle/0")
      val show = route(app, request).get
      status(show) mustBe OK

    }
  }
}