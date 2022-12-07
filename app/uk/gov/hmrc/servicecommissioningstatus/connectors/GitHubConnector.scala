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

import play.api.Logging
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, HttpResponse}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.servicecommissioningstatus.config.GitHubConfig

import java.net.URL
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class GitHubConnector @Inject() (
  httpClientV2: HttpClientV2,
  gitHubConfig: GitHubConfig
)(implicit
  ec: ExecutionContext
) extends Logging {
  import HttpReads.Implicits._

  def getGithubApi(path: String)(implicit hc: HeaderCarrier): Future[Option[String]] =
    get(new URL(s"${gitHubConfig.githubApiUrl}$path"))

  def getGithubRaw(path: String)(implicit hc: HeaderCarrier): Future[Option[String]] =
    get(new URL(s"${gitHubConfig.githubRawUrl}$path"))

  private def get(url: URL)(implicit hc: HeaderCarrier): Future[Option[String]] =
    httpClientV2
      .get(url)
      .setHeader("Authorization" -> s"token ${gitHubConfig.githubToken}")
      .withProxy
      .execute[Option[HttpResponse]]
      .map(response => response.map(_.body))
}
