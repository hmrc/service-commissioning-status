package uk.gov.hmrc.servicecommissioningstatus.connectors

import com.github.tomakehurst.wiremock.client.WireMock._
import org.scalatest.OptionValues
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.{HttpClientV2Support, WireMockSupport}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class ReleasesConnectorSpec
  extends AnyWordSpec
    with Matchers
    with OptionValues
    with ScalaFutures
    with IntegrationPatience
    with HttpClientV2Support
    with WireMockSupport {


  private lazy val releasesConnector =
    new ReleasesConnector(
      httpClientV2   = httpClientV2,
      servicesConfig = new ServicesConfig(Configuration(
        "microservice.services.releases-api.port" -> wireMockPort,
        "microservice.services.releases-api.host" -> wireMockHost
      ))
    )

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  "GET getReleases" should {
    "return WhatsRunningWhereReleases for a service" in {
      stubFor(
        get(urlEqualTo("/releases-api/whats-running-where/foo"))
          .willReturn(
            aResponse()
              .withStatus(200)
              .withBody(
                """
                   {
                     "versions": [
                       {
                        "environment": "staging"
                       },
                       {
                        "environment": "qa"
                       },
                       {
                        "environment": "production"
                       }
                     ]
                  }
              """
              )
          )
      )

      val response = releasesConnector
        .getReleases("foo")
        .futureValue

      val expectedOutput = WhatsRunningWhereReleases(Seq(Release("staging"), Release("qa"), Release("production")))

      response shouldBe expectedOutput
    }



    "return WhatsRunningWhereReleases that contains Empty Seq when service not found" in {
      stubFor(
        get(urlEqualTo("/releases-api/whats-running-where/bar"))
          .willReturn(
            aResponse()
              .withStatus(404)
              .withBody("[]")
          )
      )

      val response = releasesConnector
        .getReleases("bar")
        .futureValue

      val expectedOutput = WhatsRunningWhereReleases(Seq.empty)

      response shouldBe expectedOutput
    }
  }

}
