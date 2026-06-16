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

import scala.concurrent.ExecutionContext.Implicits.global

class SlackNotificationsConnectorSpec
  extends AnyWordSpec
    with Matchers
    with OptionValues
    with ScalaFutures
    with IntegrationPatience
    with HttpClientV2Support
    with WireMockSupport:

  private def connector(testChannelOnly: Boolean) =
    SlackNotificationsConnector(
      servicesConfig = ServicesConfig(Configuration(
        "microservice.services.slack-notifications.port" -> wireMockPort,
        "microservice.services.slack-notifications.host" -> wireMockHost,
        "slack-notifications.authToken"                  -> "token",
        "slack-notifications.onlySendToTestChannel"      -> testChannelOnly
      )),
      httpClientV2 = httpClientV2
    )

  given HeaderCarrier = HeaderCarrier()

  "send" should:
    "return a response containing no errors" in:
      stubFor(
        post(urlEqualTo("/slack-notifications/v2/notification"))
          .willReturn(
            aResponse()
              .withStatus(202)
              .withBody("{}")
          )
      )

      connector(testChannelOnly = false)
        .send(SlackNotificationRequest.markedForDecommissioning("example-frontend", "timmy.test"))
        .futureValue
        .errors shouldBe List.empty[SlackNotificationError]

    "replace channel lookup with explicit slack channel when switch enabled" in:
      stubFor(
        post(urlEqualTo("/slack-notifications/v2/notification"))
          .willReturn(
            aResponse()
              .withStatus(202)
              .withBody("{}")
          )
      )

      val msg: SlackNotificationRequest =
        SlackNotificationRequest.markedForDecommissioning("example-frontend", "timmy.test")

      msg.channelLookup shouldBe ChannelLookup.ByRepo(repositoryName = "example-frontend")

      connector(testChannelOnly = true)
        .send(msg)
        .futureValue
        .errors shouldBe List.empty[SlackNotificationError]

      val expectedLookup: String =
        """{"by":"slack-channel","slackChannels":["test-alerts-channel"]}"""

      verify(
        postRequestedFor(urlEqualTo("/slack-notifications/v2/notification"))
          .withRequestBody(containing(expectedLookup))
      )
