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
import uk.gov.hmrc.servicecommissioningstatus.model.Check

import scala.concurrent.ExecutionContext.Implicits.global

class ServiceConfigsConnectorSpec
  extends AnyWordSpec
    with Matchers
    with OptionValues
    with ScalaFutures
    with IntegrationPatience
    with HttpClientV2Support
    with WireMockSupport {

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  private lazy val serviceConfigsConnector =
    new ServiceConfigsConnector(
      httpClientV2 = httpClientV2,
      servicesConfig = new ServicesConfig(Configuration(
        "microservice.services.service-configs.port" -> wireMockPort,
        "microservice.services.service-configs.host" -> wireMockHost
      ))
    )

  "GET getMDTPFrontendRoutes" should {
    "return FrontendRoutes for service" in {
      stubFor(
        get(urlEqualTo("/service-configs/frontend-route/foo"))
          .willReturn(
            aResponse()
              .withStatus(200)
              .withBody(
                """[
                  {
                    "environment": "qa",
                    "routes": [
                      {
                        "ruleConfigurationUrl": "https://github.com/hmrc/mdtp-frontend-routes/blob/main/qa/foo.conf#L2505"
                      }
                    ]
                  },
                  {
                    "environment": "production",
                    "routes": [
                      {
                        "ruleConfigurationUrl": "https://github.com/hmrc/mdtp-frontend-routes/blob/main/production/foo.conf#L2505"
                      }
                    ]
                  },
                  {
                    "environment": "staging",
                    "routes": [
                      {
                        "ruleConfigurationUrl": "https://github.com/hmrc/mdtp-frontend-routes/blob/main/staging/foo.conf#L2505"
                      }
                    ]
                  }
                ]"""
              )
          )
      )

      val response = serviceConfigsConnector
        .getMDTPFrontendRoutes("foo")
        .futureValue

      val expectedOutput = Seq(
        FrontendRoute("qa",         Seq(Routes("https://github.com/hmrc/mdtp-frontend-routes/blob/main/qa/foo.conf#L2505"))),
        FrontendRoute("production", Seq(Routes("https://github.com/hmrc/mdtp-frontend-routes/blob/main/production/foo.conf#L2505"))),
        FrontendRoute("staging",    Seq(Routes("https://github.com/hmrc/mdtp-frontend-routes/blob/main/staging/foo.conf#L2505")))
      )

      response shouldBe expectedOutput
    }

    "return empty Seq when a service has no frontend routes" in {
      stubFor(
        get(urlEqualTo("/service-configs/frontend-route/foo-non-existing"))
          .willReturn(
            aResponse()
              .withStatus(200)
              .withBody("[]")
          )
      )

      val response = serviceConfigsConnector
        .getMDTPFrontendRoutes("foo-non-existing")
        .futureValue

      val expectedOutput = Seq.empty

      response shouldBe expectedOutput
    }
  }

  "GET getGrafanaDashboard" should {
    "return Right(Present) for a service with a Grafana Dashboard" in {
      stubFor(
        get(urlEqualTo("/service-configs/grafana-dashboards/foo"))
          .willReturn(
            aResponse()
              .withStatus(200)
              .withBody(
                """
                {
                 "service": "foo",
                 "location": "https://github.com/hmrc/grafana-dashboards/blob/HEAD/src/main/scala/uk/gov/hmrc/grafanadashboards/dashboards/TeamFoo.scala#L15"
                }
              """
              )
          )
      )

      val response = serviceConfigsConnector
        .getGrafanaDashboard("foo")
        .futureValue

      val expectedOutput = Right(Check.Present("https://github.com/hmrc/grafana-dashboards/blob/HEAD/src/main/scala/uk/gov/hmrc/grafanadashboards/dashboards/TeamFoo.scala#L15"))

      response shouldBe expectedOutput
    }

    "return Left(Missing) when no Grafana Dashboard is Not Found" in {
      stubFor(
        get(urlEqualTo("/service-configs/grafana-dashboards/foo-no-grafana"))
          .willReturn(aResponse().withStatus(404)))

      val response = serviceConfigsConnector
        .getGrafanaDashboard("foo-no-grafana")
        .futureValue

      val expectedOutput = Left(Check.Missing("https://github.com/hmrc/grafana-dashboards"))

      response shouldBe expectedOutput
    }
  }

  "GET getKibanaDashboard" should {
    "return Right(Present) for a service with a Kibana Dashboard" in {
      stubFor(
        get(urlEqualTo("/service-configs/kibana-dashboards/foo"))
          .willReturn(
            aResponse()
              .withStatus(200)
              .withBody(
                """
                {
                 "service": "foo",
                 "location": "https://github.com/hmrc/kibana-dashboards/blob/HEAD/src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices/TeamFoo.scala#L10"
                }
              """
              )
          )
      )

      val response = serviceConfigsConnector
        .getKibanaDashboard("foo")
        .futureValue

      val expectedOutput = Right(Check.Present("https://github.com/hmrc/kibana-dashboards/blob/HEAD/src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices/TeamFoo.scala#L10"))

      response shouldBe expectedOutput
    }

    "return Left(Missing) when Kibana Dashboard is Not Found" in {
      stubFor(
        get(urlEqualTo("/service-configs/kibana-dashboards/foo-no-kibana"))
          .willReturn(aResponse().withStatus(404)))

      val response = serviceConfigsConnector
        .getKibanaDashboard("foo-no-kibana")
        .futureValue

      val expectedOutput = Left(Check.Missing(("https://github.com/hmrc/kibana-dashboards")))

      response shouldBe expectedOutput
    }
  }

  "GET getBuildJobs" should {
    "return Right(Present) for a service with build jobs" in {
      stubFor(
        get(urlEqualTo("/service-configs/build-jobs/foo"))
          .willReturn(
            aResponse()
              .withStatus(200)
              .withBody(
                """
                {
                 "service": "foo",
                 "location": "https://github.com/hmrc/build-jobs/blob/HEAD/jobs/live/team-foo.groovy#L480"
                }
              """
              )
          )
      )

      val response = serviceConfigsConnector
        .getBuildJobs("foo")
        .futureValue

      val expectedOutput = Right(Check.Present("https://github.com/hmrc/build-jobs/blob/HEAD/jobs/live/team-foo.groovy#L480"))

      response shouldBe expectedOutput
    }

    "return Left(Missing) when Build Jobs for service is Not Found" in {
      stubFor(
        get(urlEqualTo("/service-configs/build-jobs/foo-no-build-jobs"))
          .willReturn(aResponse().withStatus(404)))

      val response = serviceConfigsConnector
        .getBuildJobs("foo-no-build-jobs")
        .futureValue

      val expectedOutput = Left(Check.Missing("https://github.com/hmrc/build-jobs"))

      response shouldBe expectedOutput
    }
  }

  "GET getAlertConfig" should {
    "return Right(Present) for a service that has AlertConfig" in {
      stubFor(
        get(urlEqualTo("/service-configs/alert-configs/foo"))
          .willReturn(
            aResponse()
              .withStatus(200)
              .withBody(
                """
                {
                 "service": "foo",
                 "production": "true",
                 "location": "https://github.com/hmrc/alert-config/blob/HEAD/src/main/scala/uk/gov/hmrc/alertconfig/configs/TeamFoo.scala#L13"
                }
              """
              )
          )
      )

      val response = serviceConfigsConnector
        .getAlertConfig("foo")
        .futureValue

      val expectedOutput = Right(Check.Present("https://github.com/hmrc/alert-config/blob/HEAD/src/main/scala/uk/gov/hmrc/alertconfig/configs/TeamFoo.scala#L13"))

      response shouldBe expectedOutput
    }

    "return Left(Missing) when AlertConfig for service is Not Found" in {
      stubFor(
        get(urlEqualTo("/service-configs/alert-configs/foo-no-alert-config"))
          .willReturn(aResponse().withStatus(404)))

      val response = serviceConfigsConnector
        .getAlertConfig("foo-no-alert-config")
        .futureValue

      val expectedOutput = Left(Check.Missing("https://github.com/hmrc/alert-configs"))

      response shouldBe expectedOutput
    }
  }
}
