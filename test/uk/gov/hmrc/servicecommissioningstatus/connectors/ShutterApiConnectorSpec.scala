/*
 * Copyright 2023 HM Revenue & Customs
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
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import play.api.Configuration
import play.api.libs.json.Json
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.{HttpClientV2Support, WireMockSupport}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.servicecommissioningstatus.model.Environment
import uk.gov.hmrc.servicecommissioningstatus.connectors.ShutterApiResponse._

class ShutterApiConnectorSpec
  extends AnyWordSpec
    with Matchers
    with OptionValues
    with ScalaFutures
    with IntegrationPatience
    with HttpClientV2Support
    with WireMockSupport {

  private lazy val shutterApiConnector =
    new ShutterApiConnector(
      httpClientV2   = httpClientV2,
      new ServicesConfig(Configuration(
        "microservice.services.shutter-api.port" -> wireMockPort,
        "microservice.services.shutter-api.host" -> wireMockHost
      ))
    )

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  "GET getShutterPage" should {

    val env = Environment.QA
    val serviceName = "service-name"
    val serviceUrl = s"/shutter-api/${env.asString.toLowerCase}/outage-pages/$serviceName"

    val shutterApiContent = ShutterApiResponse(
      serviceName       = "service-name",
      environment       = "qa",
      outagePageUrl     = "http://outagePage.url",
      warnings          = Seq(),
    )

    "return response body as a String for a valid outage page" in {
      stubFor(
        get(urlEqualTo(serviceUrl))
          .willReturn(
            aResponse()
              .withStatus(200)
              .withBody(Json.toJson(shutterApiContent).toString())
          )
      )

      val response = shutterApiConnector
        .getShutterPage(serviceName, env)
        .futureValue

      response shouldBe shutterApiContent

      verify(
        getRequestedFor(urlEqualTo(serviceUrl))
      )
    }
  }

}
