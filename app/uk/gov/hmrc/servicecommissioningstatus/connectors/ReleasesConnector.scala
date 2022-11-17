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

import play.api.libs.json.{Reads, __}
import uk.gov.hmrc.http.{HeaderCarrier, StringContextOps}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ReleasesConnector @Inject()(
  servicesConfig: ServicesConfig,
  httpClientV2: HttpClientV2
)(implicit ec: ExecutionContext){

  private val url: String = servicesConfig.baseUrl("releases-api")

  def getReleases(serviceName: String)(implicit hc: HeaderCarrier): Future[WhatsRunningWhereReleases] = {
    implicit val r: Reads[WhatsRunningWhereReleases] = WhatsRunningWhereReleases.reads
    httpClientV2
      .get(url"$url/releases-api/whats-running-where/$serviceName")
      .execute[Option[WhatsRunningWhereReleases]]
      .map(_.getOrElse(WhatsRunningWhereReleases(Seq.empty)))
  }
}

case class Release(environment: String)

object Release {
 val reads: Reads[Release] = {
    (__ \ "environment").read[String].map(Release(_))
  }
}

case class WhatsRunningWhereReleases(versions: Seq[Release])

object WhatsRunningWhereReleases {
  val reads: Reads[WhatsRunningWhereReleases] = {
    implicit val rs: Reads[Release] = Release.reads
    (__ \ "versions").read[Seq[Release]].map(WhatsRunningWhereReleases(_))
  }
}



