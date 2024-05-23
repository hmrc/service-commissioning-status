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

import java.time.Instant
import javax.inject.Inject

import play.api.libs.functional.syntax._
import play.api.libs.json.{Reads, __}

import scala.concurrent.{ExecutionContext, Future}

import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.servicecommissioningstatus.{Environment, ServiceName}

object ReleasesConnector {
  case class Release(environment: String)

  object Release {
    val reads: Reads[Release] =
      (__ \ "environment").read[String].map(Release(_))
  }

  case class WhatsRunningWhereReleases(versions: Seq[Release])

  object WhatsRunningWhereReleases {
    val reads: Reads[WhatsRunningWhereReleases] = {
      implicit val rs: Reads[Release] = Release.reads
      (__ \ "versions").read[Seq[Release]].map(WhatsRunningWhereReleases(_))
    }
  }

  case class DeploymentTimelineEvent(
    env: Environment,
    version: String,
    userName: String,
    start: Instant,
    end: Instant,
    displayStart: Option[Instant] = None, // set on the first/last event to the actual end date rather than the end of the chart
    displayEnd: Option[Instant] = None
  )

  object DeploymentTimelineEvent {
    implicit val deploymentTimelineEventReads: Reads[DeploymentTimelineEvent] = {
      implicit val ef = Environment.format
      ((__ \ "environment").read[Environment]
        ~ (__ \ "version").read[String]
        ~ (__ \ "username").read[String]
        ~ (__ \ "start").read[Instant]
        ~ (__ \ "end").read[Instant]
        ~ (__ \ "displayStart").readNullable[Instant]
        ~ (__ \ "displayEnd").readNullable[Instant]
      )(DeploymentTimelineEvent.apply _)
    }
  }

}

class ReleasesConnector @Inject()(
  servicesConfig: ServicesConfig,
  httpClientV2: HttpClientV2
)(implicit ec: ExecutionContext){
  import HttpReads.Implicits._
  import ReleasesConnector._

  private val url: String = servicesConfig.baseUrl("releases-api")

  def getReleases(serviceName: ServiceName)(implicit hc: HeaderCarrier): Future[WhatsRunningWhereReleases] = {
    implicit val r: Reads[WhatsRunningWhereReleases] = WhatsRunningWhereReleases.reads
    httpClientV2
      .get(url"$url/releases-api/whats-running-where/${serviceName.asString}")
      .execute[Option[WhatsRunningWhereReleases]]
      .map(_.getOrElse(WhatsRunningWhereReleases(Seq.empty)))
  }

  def deploymentTimeline(
    serviceName: ServiceName
  , from       : Instant
  , to         : Instant
  )(implicit
    hc: HeaderCarrier
  ): Future[Map[Environment, Seq[DeploymentTimelineEvent]]] = {
    implicit val dtr  = DeploymentTimelineEvent.deploymentTimelineEventReads
    implicit val er = Environment.format
    httpClientV2
      .get(url"$url/releases-api/timeline/${serviceName.asString}?from=${from.toString}&to=${to.toString}")
      .execute[Map[Environment, Seq[DeploymentTimelineEvent]]]
  }
}

