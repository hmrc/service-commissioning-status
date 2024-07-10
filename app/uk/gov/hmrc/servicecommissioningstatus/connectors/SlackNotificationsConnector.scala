/*
 * Copyright 2024 HM Revenue & Customs
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

import play.api.Logger
import play.api.libs.ws.writeableOf_JsValue
import play.api.libs.functional.syntax.{toFunctionalBuilderOps, unlift}
import play.api.libs.json._
import uk.gov.hmrc.http.{HeaderCarrier, StringContextOps}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.http.HttpReads.Implicits._

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@Singleton
class SlackNotificationsConnector @Inject()(
  httpClientV2: HttpClientV2,
  servicesConfig: ServicesConfig
)(implicit
  ec: ExecutionContext
) {

  private val logger = Logger(getClass)

  private val baseUrl = servicesConfig.baseUrl("slack-notifications")
  private val token   = servicesConfig.getString("slack-notifications.authToken")

  private val onlySendToTest: Boolean = servicesConfig.getBoolean("slack-notifications.onlySendToTestChannel")

  private def replaceChannelLookup(in: SlackNotificationRequest): SlackNotificationRequest =
    if(onlySendToTest) in.copy(channelLookup = ByChannel(slackChannels = Seq("test-alerts-channel")))
    else in

  def send(request: SlackNotificationRequest)(implicit hc: HeaderCarrier): Future[SlackNotificationResponse] = {
    implicit val snrW: Writes[SlackNotificationRequest] = SlackNotificationRequest.writes
    implicit val snrR: Reads[SlackNotificationResponse] = SlackNotificationResponse.reads
    httpClientV2
      .post(url"$baseUrl/slack-notifications/v2/notification")
      .withBody(Json.toJson(replaceChannelLookup(request)))
      .setHeader("Authorization" -> token)
      .execute[SlackNotificationResponse]
      .recoverWith {
        case NonFatal(ex) =>
          logger.error(s"Unable to notify ${request.channelLookup} on Slack", ex)
          Future.failed(ex)
      }
  }

}

sealed trait ChannelLookup

final case class ByRepo(
  by: String = "github-repository",
  repositoryName: String
) extends ChannelLookup

object ByRepo {
  val writes: OWrites[ByRepo] =
    ( (__ \ "by"            ).write[String]
    ~ (__ \ "repositoryName").write[String]
    )(b => Tuple.fromProductTyped(b))
}

final case class ByChannel(
  by: String = "slack-channel",
  slackChannels: Seq[String]
) extends ChannelLookup

object ByChannel {
  val writes: OWrites[ByChannel] =
    ( (__ \ "by"           ).write[String]
    ~ (__ \ "slackChannels").write[Seq[String]]
    )(b => Tuple.fromProductTyped(b))
}

object ChannelLookup {
  val writes: Writes[ChannelLookup] = {
    case lookup: ByRepo    => Json.toJson(lookup)(ByRepo.writes)
    case lookup: ByChannel => Json.toJson(lookup)(ByChannel.writes)
  }
}

final case class SlackNotificationRequest(
  channelLookup: ChannelLookup,
  displayName  : String,
  emoji        : String,
  text         : String,
  blocks       : Seq[JsObject]
)

object SlackNotificationRequest {
  val writes: Writes[SlackNotificationRequest] =
    ( (__ \ "channelLookup").write[ChannelLookup](ChannelLookup.writes)
    ~ (__ \ "displayName"  ).write[String]
    ~ (__ \ "emoji"        ).write[String]
    ~ (__ \ "text"         ).write[String]
    ~ (__ \ "blocks"       ).write[Seq[JsObject]]
    )(s => Tuple.fromProductTyped(s))

  def markedForDecommissioning(repositoryName: String, username: String): SlackNotificationRequest = {
    val blocks = Seq(
      Json.obj(
        "type" -> JsString("section"),
        "text" -> Json.obj(
          "type" -> JsString("mrkdwn"),
          "text" -> JsString(s"@$username has marked `$repositoryName` for decommissioning in the MDTP Catalogue.\n\nDecommissioning progress can be tracked <https://catalogue.tax.service.gov.uk/service/$repositoryName/commissioning-state|here>.\n\nIf this was a mistake, please contact #team-platops")
        )
      )
    )

    SlackNotificationRequest(
      channelLookup = ByRepo(repositoryName = repositoryName),
      displayName   = "MDTP Catalogue",
      emoji         = ":tudor-crown:",
      text          = s"$repositoryName has been marked for decommissioning.",
      blocks        = blocks
    )
  }
}

final case class SlackNotificationError(
  code   : String,
  message: String
)

final case class SlackNotificationResponse(
  errors: List[SlackNotificationError]
)

object SlackNotificationResponse {
  val reads: Reads[SlackNotificationResponse] = {
    implicit val sneReads: Reads[SlackNotificationError] =
      ( (__ \ "code"   ).read[String]
      ~ (__ \ "message").read[String]
      )(SlackNotificationError.apply _)

    (__ \ "errors")
      .readWithDefault[List[SlackNotificationError]](List.empty)
      .map(SlackNotificationResponse.apply)
  }
}
