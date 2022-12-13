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
import uk.gov.hmrc.servicecommissioningstatus.model.{Check, Environment}
import uk.gov.hmrc.servicecommissioningstatus.service.StatusCheckService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ServiceStatusControllerSpec
  extends AnyWordSpec
    with Matchers
    with MockitoSugar {

  private val mockStatusCheckService: StatusCheckService = mock[StatusCheckService]

  import Environment._
  private val appConfigEnvironment: Map[Environment, Check.Result] =
    Map(
      Integration   -> Left( Check.Missing("https://github.com/hmrc/app-config-development")),
      Development   -> Right(Check.Present("https://github.com/hmrc/app-config-development/blob/main/foo.yaml")),
      Production    -> Right(Check.Present("https://github.com/hmrc/app-config-production/blob/main/foo.yaml")),
      Staging       -> Right(Check.Present("https://github.com/hmrc/app-config-staging/blob/main/foo.yaml")),
      QA            -> Right(Check.Present("https://github.com/hmrc/app-config-qa/blob/main/foo.yaml")),
      ExternalTest  -> Right(Check.Present("https://github.com/hmrc/app-config-externaltest/blob/main/foo.yaml"))
    )

   import Check._
   private val checks =
      SimpleCheck(title = "Github Repo"           , result  = Right(Check.Present("https://github.com/hmrc/foo"))) ::
      SimpleCheck(title = "App Config Base"       , result  = Left( Check.Missing("https://github.com/hmrc/app-config-base/blob/main/foo.conf"))) ::
      EnvCheck   (title = "App Config Environment", results = appConfigEnvironment) ::
      Nil

  "ServiceStatusController" should {
    "return all completed Service Commissioning Checks as Json" in {
      when(mockStatusCheckService.commissioningStatusChecks(eqTo("foo"))(any[HeaderCarrier]))
        .thenReturn(Future.successful(checks))

      val controller = new ServiceStatusController(Helpers.stubControllerComponents(), mockStatusCheckService)
      val result = controller.statusChecks("foo")(FakeRequest())
      val bodyText = contentAsJson(result)
      bodyText mustBe Json.parse(json1)
    }
  }

  private val json1 = """
    [{
      "title": "Github Repo",
      "simpleCheck": { "evidence": "https://github.com/hmrc/foo" }
    }, {
      "title": "App Config Base",
      "simpleCheck": { "add"     : "https://github.com/hmrc/app-config-base/blob/main/foo.conf" }
    }, {
      "title": "App Config Environment",
      "environmentCheck": {
        "integration":  { "add":      "https://github.com/hmrc/app-config-development" },
        "development":  { "evidence": "https://github.com/hmrc/app-config-development/blob/main/foo.yaml" },
        "production":   { "evidence": "https://github.com/hmrc/app-config-production/blob/main/foo.yaml" },
        "staging":      { "evidence": "https://github.com/hmrc/app-config-staging/blob/main/foo.yaml" },
        "qa":           { "evidence": "https://github.com/hmrc/app-config-qa/blob/main/foo.yaml" },
        "externaltest": { "evidence": "https://github.com/hmrc/app-config-externaltest/blob/main/foo.yaml" }
      }
    }]"""

}
