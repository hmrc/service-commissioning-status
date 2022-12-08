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
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import play.api.Configuration
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.{HttpClientV2Support, WireMockSupport}
import uk.gov.hmrc.servicecommissioningstatus.config.GitHubConfig

import scala.concurrent.ExecutionContext.Implicits.global

class GitHubConnectorSpec
  extends AnyWordSpec
    with Matchers
    with OptionValues
    with ScalaFutures
    with IntegrationPatience
    with HttpClientV2Support
    with WireMockSupport {


  private lazy val githubConnector =
    new GitHubConnector(
      httpClientV2   = httpClientV2,
      gitHubConfig = new GitHubConfig(Configuration(
        "github.open.api.rawurl" -> wireMockUrl,
        "github.open.api.url"    -> wireMockUrl,
        "github.open.api.token"  -> "test-token"
      ))
    )

  implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  "GET getGithubApi" should {
    "return response body as a String for a valid repo" in {
      stubFor(
        get(urlEqualTo("/hmrc/repos/foo"))
          .willReturn(
            aResponse()
              .withStatus(200)
              .withBody("Repository Content")
          )
      )

      val response = githubConnector
        .getGithubApi("/hmrc/repos/foo")
        .futureValue
        .value

      response shouldBe "Repository Content"

      verify(
        getRequestedFor(urlEqualTo("/hmrc/repos/foo"))
          .withHeader("Authorization", equalTo("token test-token"))
      )
    }

    "return None when repo Not Found" in {
      stubFor(
        get(urlEqualTo("/hmrc/repos/foo-non-existing"))
          .willReturn(aResponse().withStatus(404)))

      val response = githubConnector
        .getGithubApi("/hmrc/repos/foo-non-existing")
        .futureValue

      response shouldBe None

      verify(
        getRequestedFor(urlEqualTo("/hmrc/repos/foo-non-existing"))
          .withHeader("Authorization", equalTo("token test-token"))
      )
    }
  }

  "GET getGithubRaw" should {
    "return response body as a String for a valid repo" in {
      stubFor(
        get(urlEqualTo("/hmrc/repos/foo"))
          .willReturn(
            aResponse()
              .withStatus(200)
              .withBody("Raw Repository Content")
          )
      )

      val response = githubConnector
        .getGithubRaw("/hmrc/repos/foo")
        .futureValue
        .value

      response shouldBe "Raw Repository Content"

      verify(
        getRequestedFor(urlEqualTo("/hmrc/repos/foo"))
          .withHeader("Authorization", equalTo("token test-token"))
      )
    }

    "return None when repo Not Found" in {
      stubFor(
        get(urlEqualTo("/hmrc/repos/foo-non-existing"))
          .willReturn(aResponse().withStatus(404)))

      val response = githubConnector
        .getGithubRaw("/hmrc/repos/foo-non-existing")
        .futureValue

      response shouldBe None

      verify(
        getRequestedFor(urlEqualTo("/hmrc/repos/foo-non-existing"))
          .withHeader("Authorization", equalTo("token test-token"))
      )
    }
  }
}
