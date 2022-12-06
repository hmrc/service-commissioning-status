package uk.gov.hmrc.servicecommissioningstatus.connectors

import com.github.tomakehurst.wiremock.client.WireMock._
import org.scalatest.wordspec.AnyWordSpec
import org.mockito.MockitoSugar
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.http.test.WireMockSupport

class GitHubConnectorSpec
  extends AnyWordSpec
    with MockitoSugar
    with Matchers
    with ScalaFutures
    with WireMockSupport
    with GuiceOneServerPerSuite {


  val token = "token"

  override def fakeApplication(): Application =
    new GuiceApplicationBuilder()
      .configure(
        "github.open.api.user"  -> "user",
        "github.open.api.key"         -> token,
        "github.open.api.url"         -> wireMockUrl,
        "github.open.api.rawurl"      -> s"$wireMockUrl/raw",
        "metrics.jvm"                 -> false,
      )
      .build()

  private val connector = app.injector.instanceOf[GitHubConnector]


}
