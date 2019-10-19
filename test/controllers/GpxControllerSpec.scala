package controllers

import models.{GpxRep, MaterielRep, TrekMaterielRep}
import org.mockito.Mock
import play.api.mvc._
import play.api.i18n._
import org.scalatestplus.play._
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import play.api.http.FileMimeTypes
import play.api.test._
import play.api.test.Helpers._
import play.api.test.CSRFTokenHelper._
import org.scalatest.mockito.MockitoSugar

import scala.concurrent.ExecutionContext


/**
 * Gpx form controller specs
 */
class GpxControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with MockitoSugar{

  // Provide stubs for components based off Helpers.stubControllerComponents()
  class StubComponents(cc:ControllerComponents = stubControllerComponents()) extends MessagesControllerComponents {
    override val parsers: PlayBodyParsers = cc.parsers
    override val messagesApi: MessagesApi = cc.messagesApi
    override val langs: Langs = cc.langs
    override val fileMimeTypes: FileMimeTypes = cc.fileMimeTypes
    override val executionContext: ExecutionContext = cc.executionContext
    override val actionBuilder: ActionBuilder[Request, AnyContent] = cc.actionBuilder
    override val messagesActionBuilder: MessagesActionBuilder = new DefaultMessagesActionBuilderImpl(parsers.default, messagesApi)(executionContext)
  }

  "GpxController GET" should {

    "render the index page from a new instance of controller" in {
//      val repoGpx = Mock[GpxRep]
//      val repoTrekMateriels = Mock[TrekMaterielRep]
//      val repoMateriels = Mock[MaterielRep]
//      val controller = new GpxController(repoGpx, repoTrekMateriels, repoMateriels, new StubComponents())
//      val request = FakeRequest().withCSRFToken
//      val home = controller.newGpx("T").apply(request)
//
//      status(home) mustBe OK
//      contentType(home) mustBe Some("text/html")
    }

    "render the index page from the application" in {
      val controller = inject[GpxController]
      val request = FakeRequest().withCSRFToken
      val home = controller.newGpx("T").apply(request)

      status(home) mustBe OK
      contentType(home) mustBe Some("text/html")
    }

    "render the index page from the router" in {
      val request = CSRFTokenHelper.addCSRFToken(FakeRequest(GET, "/derp"))
      val home = route(app, request).get

      status(home) mustBe OK
      contentType(home) mustBe Some("text/html")
    }
  }

  "GpxController POST" should {
    "process form" in {
      val request = {
        FakeRequest(POST, "/gpx")
          .withFormUrlEncodedBody("name" -> "play", "age" -> "4")
      }
      val home = route(app, request).get

      status(home) mustBe SEE_OTHER
    }
  }
}
