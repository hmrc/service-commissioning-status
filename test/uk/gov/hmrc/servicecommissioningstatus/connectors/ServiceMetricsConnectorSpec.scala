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
import org.scalatest.OptionValues
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.{HttpClientV2Support, WireMockSupport}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.servicecommissioningstatus.connectors.ServiceMetricsConnector.MongoCollectionSize
import uk.gov.hmrc.servicecommissioningstatus.{Environment, ServiceName}

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

class ServiceMetricsConnectorSpec
  extends AnyWordSpec
    with Matchers
    with OptionValues
    with ScalaFutures
    with IntegrationPatience
    with HttpClientV2Support
    with WireMockSupport:

  private lazy val connector =
    new ServiceMetricsConnector(
      servicesConfig = new ServicesConfig(Configuration(
        "microservice.services.service-metrics.port" -> wireMockPort,
        "microservice.services.service-metrics.host" -> wireMockHost
      )),
      httpClientV2 = httpClientV2
    )

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  "getCollections" should:
    "return a list of collections for a given service" in:
      val rawResponse: String =
        """
          |[
          |   {
          |      "database":"service-one",
          |      "collection":"collection-one",
          |      "sizeBytes":1000,
          |      "date":"2023-02-02",
          |      "environment":"qa",
          |      "service":"service-one"
          |   },
          |   {
          |      "database":"service-one",
          |      "collection":"collection-one",
          |      "sizeBytes":5000,
          |      "date":"2023-02-02",
          |      "environment":"staging",
          |      "service":"service-one"
          |   },
          |   {
          |      "database":"service-one",
          |      "collection":"collection-one",
          |      "sizeBytes":8500,
          |      "date":"2023-02-02",
          |      "environment":"production",
          |      "service":"service-one"
          |   }
          |]
          |""".stripMargin


      stubFor(
        get(urlEqualTo("/service-metrics/service-one/collections"))
          .willReturn(
            aResponse()
              .withStatus(200)
              .withBody(rawResponse)
          )
      )

      val expected: Seq[MongoCollectionSize] =
        Seq(
          MongoCollectionSize(
            database    = "service-one",
            collection  = "collection-one",
            sizeBytes   = BigDecimal(1000),
            date        = LocalDate.of(2023, 2, 2),
            environment = Environment.QA,
            service     = Some("service-one")
          ),
          MongoCollectionSize(
            database = "service-one",
            collection = "collection-one",
            sizeBytes = BigDecimal(5000),
            date = LocalDate.of(2023, 2, 2),
            environment = Environment.Staging,
            service = Some("service-one")
          ),
          MongoCollectionSize(
            database = "service-one",
            collection = "collection-one",
            sizeBytes = BigDecimal(8500),
            date = LocalDate.of(2023, 2, 2),
            environment = Environment.Production,
            service = Some("service-one")
          )
        )

      connector.getCollections(ServiceName("service-one")).futureValue should contain theSameElementsAs expected
