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

package uk.gov.hmrc.servicecommissioningstatus.controllers

import org.mockito.scalatest.MockitoSugar
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.servicecommissioningstatus.model.Environment.{Development, ExternalTest, Integration, Production, QA, Staging}
import uk.gov.hmrc.servicecommissioningstatus.model.{AppConfigEnvironment, Dashboard, DeploymentEnvironment, FrontendRoutes, ServiceCommissioningStatus, StatusCheck}
import uk.gov.hmrc.servicecommissioningstatus.service.StatusCheckService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ServiceStatusControllerSpec
  extends AnyWordSpec
    with Matchers
    with MockitoSugar {

  private val mockStatusCheckService: StatusCheckService = mock[StatusCheckService]

  private val frontendRoutes = FrontendRoutes(
    Map(
      Integration   -> StatusCheck(None),
      Development   -> StatusCheck(Some("https://github.com/hmrc/mdtp-frontend-routes/blob/HEAD/development/frontend-proxy-application-rules.conf#L5428")),
      Production    -> StatusCheck(Some("https://github.com/hmrc/mdtp-frontend-routes/blob/HEAD/production/frontend-proxy-application-rules.conf#L5009")),
      Staging       -> StatusCheck(Some("https://github.com/hmrc/mdtp-frontend-routes/blob/HEAD/staging/frontend-proxy-application-rules.conf#L5833")),
      QA            -> StatusCheck(Some("https://github.com/hmrc/mdtp-frontend-routes/blob/HEAD/qa/frontend-proxy-application-rules.conf#L6254")),
      ExternalTest  -> StatusCheck(Some("https://github.com/hmrc/mdtp-frontend-routes/blob/HEAD/externaltest/frontend-proxy-application-rules.conf#L2819"))
    )
  )

  private val appConfigEnvironment = AppConfigEnvironment(
    Map(
      Integration   -> StatusCheck(None),
      Development   -> StatusCheck(Some("https://github.com/hmrc/app-config-Development/blob/main/foo.yaml")),
      Production    -> StatusCheck(Some("https://github.com/hmrc/app-config-Production/blob/main/foo.yaml")),
      Staging       -> StatusCheck(Some("https://github.com/hmrc/app-config-Staging/blob/main/foo.yaml")),
      QA            -> StatusCheck(Some("https://github.com/hmrc/app-config-QA/blob/main/foo.yaml")),
      ExternalTest  -> StatusCheck(Some("https://github.com/hmrc/app-config-ExternalTest/blob/main/foo.yaml"))
    )
  )

  private val deploymentEnvironment = DeploymentEnvironment(
    Map(
      Integration   -> StatusCheck(None),
      Development   -> StatusCheck(Some("https://catalogue.tax.service.gov.uk/deployment-timeline?service=foo")),
      Production    -> StatusCheck(Some("https://catalogue.tax.service.gov.uk/deployment-timeline?service=foo")),
      Staging       -> StatusCheck(Some("https://catalogue.tax.service.gov.uk/deployment-timeline?service=foo")),
      QA            -> StatusCheck(Some("https://catalogue.tax.service.gov.uk/deployment-timeline?service=foo")),
      ExternalTest  -> StatusCheck(Some("https://catalogue.tax.service.gov.uk/deployment-timeline?service=foo"))
    )
  )

  private val dashboard = Dashboard(
    kibana = StatusCheck(Some("https://github.com/hmrc/kibana-dashboards/blob/HEAD/src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices/TeamFoo.scala#L10")),
    grafana = StatusCheck(Some("https://github.com/hmrc/grafana-dashboards/blob/HEAD/src/main/scala/uk/gov/hmrc/grafanadashboards/dashboards/TeamFoo.scala#L15"))
  )



  private val serviceCommissioningStatus = ServiceCommissioningStatus(
    serviceName         = "foo"
    , hasRepo           = StatusCheck(Some("https://github.com/hmrc/foo"))
    , hasSMConfig       = StatusCheck(Some("https://github.com/hmrc/service-manager-config/blob/main/services.json#L28887"))
    , hasFrontendRoutes = frontendRoutes
    , hasAppConfigBase  = StatusCheck(Some("https://github.com/hmrc/app-config-base/blob/main/foo.conf"))
    , hasAppConfigEnv   = appConfigEnvironment
    , isDeployed        = deploymentEnvironment
    , hasDashboards     = dashboard
    , hasBuildJobs      = StatusCheck(Some("https://github.com/hmrc/build-jobs/blob/HEAD/jobs/live/teamfoo.groovy#L480"))
    , hasAlerts         = StatusCheck(Some("https://github.com/hmrc/alert-config/blob/HEAD/src/main/scala/uk/gov/hmrc/alertconfig/configs/TeamFoo.scala#L13"))
  )

  "ServiceStatusController" should {
    "return all completed Service Commissioning Checks as Json" in {
      when(mockStatusCheckService.commissioningStatusChecks(eqTo("foo"))(any[HeaderCarrier]))
        .thenReturn(Future.successful(serviceCommissioningStatus))

      val controller = new ServiceStatusController(Helpers.stubControllerComponents(), mockStatusCheckService)
      val result = controller.statusChecks("foo")(FakeRequest())
      val bodyText = contentAsJson(result)
      bodyText mustBe Json.parse(json1)
    }
  }

  private val json1 =
    """{
       "serviceName":"foo",
       "hasRepo":{
          "evidence":"https://github.com/hmrc/foo",
          "status":true
       },
       "hasSMConfig":{
          "evidence":"https://github.com/hmrc/service-manager-config/blob/main/services.json#L28887",
          "status":true
       },
       "hasFrontendRoutes":{
          "integration":{
             "status":false
          },
          "development":{
             "evidence":"https://github.com/hmrc/mdtp-frontend-routes/blob/HEAD/development/frontend-proxy-application-rules.conf#L5428",
             "status":true
          },
          "production":{
             "evidence":"https://github.com/hmrc/mdtp-frontend-routes/blob/HEAD/production/frontend-proxy-application-rules.conf#L5009",
             "status":true
          },
          "staging":{
             "evidence":"https://github.com/hmrc/mdtp-frontend-routes/blob/HEAD/staging/frontend-proxy-application-rules.conf#L5833",
             "status":true
          },
          "qa":{
             "evidence":"https://github.com/hmrc/mdtp-frontend-routes/blob/HEAD/qa/frontend-proxy-application-rules.conf#L6254",
            "status":true
          },
          "externaltest":{
             "evidence":"https://github.com/hmrc/mdtp-frontend-routes/blob/HEAD/externaltest/frontend-proxy-application-rules.conf#L2819",
             "status":true
          }
       },
       "hasAppConfigBase":{
          "evidence":"https://github.com/hmrc/app-config-base/blob/main/foo.conf",
          "status":true
       },
       "hasAppConfigEnv":{
          "integration":{
             "status":false
          },
          "development":{
             "evidence":"https://github.com/hmrc/app-config-Development/blob/main/foo.yaml",
             "status":true
          },
          "production":{
             "evidence":"https://github.com/hmrc/app-config-Production/blob/main/foo.yaml",
             "status":true
          },
          "staging":{
             "evidence":"https://github.com/hmrc/app-config-Staging/blob/main/foo.yaml",
             "status":true
          },
          "qa":{
             "evidence":"https://github.com/hmrc/app-config-QA/blob/main/foo.yaml",
             "status":true
          },
          "externaltest":{
             "evidence":"https://github.com/hmrc/app-config-ExternalTest/blob/main/foo.yaml",
             "status":true
          }
       },
       "isDeployedIn":{
          "integration":{
             "status":false
          },
          "development":{
             "evidence":"https://catalogue.tax.service.gov.uk/deployment-timeline?service=foo",
             "status":true
          },
          "production":{
             "evidence":"https://catalogue.tax.service.gov.uk/deployment-timeline?service=foo",
             "status":true
          },
          "staging":{
             "evidence":"https://catalogue.tax.service.gov.uk/deployment-timeline?service=foo",
             "status":true
          },
          "qa":{
             "evidence":"https://catalogue.tax.service.gov.uk/deployment-timeline?service=foo",
             "status":true
          },
          "externaltest":{
             "evidence":"https://catalogue.tax.service.gov.uk/deployment-timeline?service=foo",
             "status":true
          }
       },
       "hasDashboards":{
          "kibana":{
             "evidence":"https://github.com/hmrc/kibana-dashboards/blob/HEAD/src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices/TeamFoo.scala#L10",
             "status":true
          },
          "grafana":{
             "evidence":"https://github.com/hmrc/grafana-dashboards/blob/HEAD/src/main/scala/uk/gov/hmrc/grafanadashboards/dashboards/TeamFoo.scala#L15",
             "status":true
          }
       },
       "hasBuildJobs":{
          "evidence":"https://github.com/hmrc/build-jobs/blob/HEAD/jobs/live/teamfoo.groovy#L480",
          "status":true
       },
       "hasAlerts":{
          "evidence":"https://github.com/hmrc/alert-config/blob/HEAD/src/main/scala/uk/gov/hmrc/alertconfig/configs/TeamFoo.scala#L13",
          "status":true
       }
      }"""
}
