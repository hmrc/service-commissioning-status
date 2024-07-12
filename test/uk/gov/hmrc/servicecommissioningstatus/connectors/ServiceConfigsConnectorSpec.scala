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
import uk.gov.hmrc.servicecommissioningstatus.{Environment, ServiceName}

import scala.concurrent.ExecutionContext.Implicits.global

class ServiceConfigsConnectorSpec
  extends AnyWordSpec
    with Matchers
    with OptionValues
    with ScalaFutures
    with IntegrationPatience
    with HttpClientV2Support
    with WireMockSupport:
  import ServiceConfigsConnector.{FrontendRoute, Routes}

  given HeaderCarrier = HeaderCarrier()

  private lazy val serviceConfigsConnector: ServiceConfigsConnector =
    ServiceConfigsConnector(
      httpClientV2 = httpClientV2,
      servicesConfig = ServicesConfig(Configuration(
        "microservice.services.service-configs.port" -> wireMockPort,
        "microservice.services.service-configs.host" -> wireMockHost
      ))
    )

  "GET getMDTPFrontendRoutes" should:
    "return FrontendRoutes for service" in:
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

      val response: Seq[FrontendRoute] =
        serviceConfigsConnector
          .getMDTPFrontendRoutes(ServiceName("foo"))
          .futureValue

      val expectedOutput: Seq[FrontendRoute] =
        Seq(
          FrontendRoute(Environment.QA,         Seq(Routes("https://github.com/hmrc/mdtp-frontend-routes/blob/main/qa/foo.conf#L2505"))),
          FrontendRoute(Environment.Production, Seq(Routes("https://github.com/hmrc/mdtp-frontend-routes/blob/main/production/foo.conf#L2505"))),
          FrontendRoute(Environment.Staging,    Seq(Routes("https://github.com/hmrc/mdtp-frontend-routes/blob/main/staging/foo.conf#L2505")))
        )

      response shouldBe expectedOutput

    "return empty Seq when a service has no frontend routes" in:
      stubFor(
        get(urlEqualTo("/service-configs/frontend-route/foo-non-existing"))
          .willReturn(
            aResponse()
              .withStatus(200)
              .withBody("[]")
          )
      )

      val response: Seq[FrontendRoute] =
        serviceConfigsConnector
          .getMDTPFrontendRoutes(ServiceName("foo-non-existing"))
          .futureValue

      val expectedOutput = Seq.empty

      response shouldBe expectedOutput

  "GET getConfigLocation" should:
    "return Right(Present) for a service with a Grafana Dashboard" in:
      stubFor(get(urlEqualTo("/service-configs/services/foo/config-location"))
        .willReturn(aResponse().withStatus(200).withBody(
          """{
            "app-config-production":   "https://github.com/hmrc/app-config-production/blob/HEAD/foo.yaml",
            "grafana":                 "https://github.com/hmrc/grafana-dashboards/blob/HEAD/src/main/scala/uk/gov/hmrc/grafanadashboards/dashboards/some-team.scala#L59",
            "build-jobs":              "https://github.com/hmrc/build-jobs/blob/HEAD/jobs/live/some-team.groovy#L51",
            "app-config-staging":      "https://github.com/hmrc/app-config-staging/blob/HEAD/foo.yaml",
            "outage-page-development": "https://github.com/hmrc/outage-pages/blob/main/development",
            "app-config-development":  "https://github.com/hmrc/app-config-development/blob/HEAD/foo.yaml",
            "outage-page-staging":     "https://github.com/hmrc/outage-pages/blob/main/staging",
            "alerts":                  "https://github.com/hmrc/alert-config/blob/HEAD/src/main/scala/uk/gov/hmrc/alertconfig/configs/some-team.scala#L164",
            "app-config-base":         "https://github.com/hmrc/app-config-base/blob/HEAD/foo.conf",
            "outage-page-qa":          "https://github.com/hmrc/outage-pages/blob/main/qa",
            "kibana":                  "https://github.com/hmrc/kibana-dashboards/blob/HEAD/src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices/some-team.scala#L8",
            "outage-page-production":  "https://github.com/hmrc/outage-pages/blob/main/production",
            "app-config-qa":           "https://github.com/hmrc/app-config-qa/blob/HEAD/foo.yaml",
            "service-manager-config":  "https://github.com/hmrc/service-manager-config/blob/main/services.json#L11428"
          }"""
        )))

      serviceConfigsConnector.getConfigLocation(ServiceName("foo")).futureValue shouldBe Map(
        "app-config-production"   -> "https://github.com/hmrc/app-config-production/blob/HEAD/foo.yaml",
        "grafana"                 -> "https://github.com/hmrc/grafana-dashboards/blob/HEAD/src/main/scala/uk/gov/hmrc/grafanadashboards/dashboards/some-team.scala#L59",
        "build-jobs"              -> "https://github.com/hmrc/build-jobs/blob/HEAD/jobs/live/some-team.groovy#L51",
        "app-config-staging"      -> "https://github.com/hmrc/app-config-staging/blob/HEAD/foo.yaml",
        "outage-page-development" -> "https://github.com/hmrc/outage-pages/blob/main/development",
        "app-config-development"  -> "https://github.com/hmrc/app-config-development/blob/HEAD/foo.yaml",
        "outage-page-staging"     -> "https://github.com/hmrc/outage-pages/blob/main/staging",
        "alerts"                  -> "https://github.com/hmrc/alert-config/blob/HEAD/src/main/scala/uk/gov/hmrc/alertconfig/configs/some-team.scala#L164",
        "app-config-base"         -> "https://github.com/hmrc/app-config-base/blob/HEAD/foo.conf",
        "outage-page-qa"          -> "https://github.com/hmrc/outage-pages/blob/main/qa",
        "kibana"                  -> "https://github.com/hmrc/kibana-dashboards/blob/HEAD/src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices/some-team.scala#L8",
        "outage-page-production"  -> "https://github.com/hmrc/outage-pages/blob/main/production",
        "app-config-qa"           -> "https://github.com/hmrc/app-config-qa/blob/HEAD/foo.yaml",
        "service-manager-config"  -> "https://github.com/hmrc/service-manager-config/blob/main/services.json#L11428"
      )
