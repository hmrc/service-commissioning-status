/*
 * Copyright 2022 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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



    "return WhatsRunningWhereReleases that contains Empty Seq when service Not Found" in {
      stubFor(
        get(urlEqualTo("/releases-api/whats-running-where/foo-non-existing"))
          .willReturn(aResponse().withStatus(404)))

      val response = releasesConnector
        .getReleases("foo-non-existing")
        .futureValue

      val expectedOutput = WhatsRunningWhereReleases(Seq.empty)

      response shouldBe expectedOutput
    }
  }

}
