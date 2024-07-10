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

package uk.gov.hmrc.servicecommissioningstatus.controllers

import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import org.mockito.ArgumentMatchers.{eq => eqTo, *}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import play.api.test.Helpers.*
import play.api.test.{FakeRequest, Helpers}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.servicecommissioningstatus.{Check, Environment, Result, ServiceName}
import uk.gov.hmrc.servicecommissioningstatus.service.StatusCheckService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class ServiceStatusControllerSpec
  extends AnyWordSpec
    with Matchers
    with MockitoSugar {

  private val mockStatusCheckService: StatusCheckService = mock[StatusCheckService]

  import Environment._
  private val appConfigEnvironment: Map[Environment, Result] =
    Map(
      Integration   -> Result.Missing("https://github.com/hmrc/app-config-development"),
      Development   -> Result.Present("https://github.com/hmrc/app-config-development/blob/main/foo.yaml"),
      Production    -> Result.Present("https://github.com/hmrc/app-config-production/blob/main/foo.yaml"),
      Staging       -> Result.Present("https://github.com/hmrc/app-config-staging/blob/main/foo.yaml"),
      QA            -> Result.Present("https://github.com/hmrc/app-config-qa/blob/main/foo.yaml"),
      ExternalTest  -> Result.Present("https://github.com/hmrc/app-config-externaltest/blob/main/foo.yaml")
    )

   import Check._
   private val checks =
      SimpleCheck(
        title      = "Github Repo",
        result     = Result.Present("https://github.com/hmrc/foo"),
        helpText   = "Github help text",
        linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook")
      ) ::
      SimpleCheck(
        title      = "App Config Base",
        result     = Result.Missing("https://github.com/hmrc/app-config-base/blob/main/foo.conf"),
        helpText   = "Base help text",
        linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook")
      ) ::
      EnvCheck(
        title      = "App Config Environment",
        results    = appConfigEnvironment,
        helpText   = "Env help text",
        linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook")
      ) ::
      Nil

  private val json1 =
    """
    [{
      "title": "Github Repo",
      "simpleCheck": { "evidence": "https://github.com/hmrc/foo" },
      "helpText": "Github help text",
      "linkToDocs": "https://docs.tax.service.gov.uk/mdtp-handbook"
    }, {
      "title": "App Config Base",
      "simpleCheck": { "add"     : "https://github.com/hmrc/app-config-base/blob/main/foo.conf" },
      "helpText": "Base help text",
      "linkToDocs": "https://docs.tax.service.gov.uk/mdtp-handbook"
    }, {
      "title": "App Config Environment",
      "environmentCheck": {
        "integration":  { "add":      "https://github.com/hmrc/app-config-development" },
        "development":  { "evidence": "https://github.com/hmrc/app-config-development/blob/main/foo.yaml" },
        "production":   { "evidence": "https://github.com/hmrc/app-config-production/blob/main/foo.yaml" },
        "staging":      { "evidence": "https://github.com/hmrc/app-config-staging/blob/main/foo.yaml" },
        "qa":           { "evidence": "https://github.com/hmrc/app-config-qa/blob/main/foo.yaml" },
        "externaltest": { "evidence": "https://github.com/hmrc/app-config-externaltest/blob/main/foo.yaml" }
      },
      "helpText": "Env help text",
      "linkToDocs": "https://docs.tax.service.gov.uk/mdtp-handbook"
    }]"""

  "LifecycleStatusController" should {
    "return all completed Service Commissioning Checks as Json" in {
      when(mockStatusCheckService.commissioningStatusChecks(ServiceName(eqTo("foo")))(any[HeaderCarrier], any[ExecutionContext]))
        .thenReturn(Future.successful(checks))

      val controller = LifecycleStatusController(Helpers.stubControllerComponents(), mockStatusCheckService)
      val result = controller.statusChecks(ServiceName("foo"))(FakeRequest())
      val bodyText = contentAsJson(result)
      bodyText mustBe Json.parse(json1)
    }
  }
}
