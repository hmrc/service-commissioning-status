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

import akka.stream.Materializer
import akka.util.ByteString
import akka.stream.scaladsl.{Source, StreamConverters}
import play.api.Logging
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, HttpResponse, UpstreamErrorResponse}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.servicecommissioningstatus.config.GitHubConfig

import java.io.InputStream
import java.net.URL
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}


class GitHubConnector @Inject() (
  httpClientV2: HttpClientV2,
  gitHubConfig: GitHubConfig
)(implicit
  ec: ExecutionContext,
  materializer: Materializer)
  extends Logging {
  import HttpReads.Implicits._


  def getGithubApi(path: String)(implicit hc: HeaderCarrier): Future[Option[String]] = {
    val requestUrl = new URL(s"${gitHubConfig.githubApiUrl}$path")
    httpClientV2
      .get(requestUrl)
      .setHeader("Authorization" -> s"token ${gitHubConfig.githubToken}")
      .withProxy
      .execute[Option[HttpResponse]]
      .map(response => response.map(_.body))
  }

  def getGithubRaw(path: String)(implicit hc: HeaderCarrier): Future[Option[String]] = {
    val requestUrl = new URL(s"${gitHubConfig.githubRawUrl}$path")
    httpClientV2
      .get(requestUrl)
      .setHeader("Authorization" -> s"token ${gitHubConfig.githubToken}")
      .withProxy
      .execute[Option[HttpResponse]]
      .map(response => response.map(_.body))
  }

  def streamGithubCodeLoad(path: String)(implicit hc: HeaderCarrier): Future[Option[InputStream]] = {
    logger.info(s"Getting code archive from: https://codeload.github.com$path")
    httpClientV2
      .get(new URL(s"https://codeload.github.com$path"))
      .setHeader("Authorization" -> s"token ${gitHubConfig.githubToken}")
      .withProxy
      .stream[Either[UpstreamErrorResponse, Source[ByteString, _]]]
      .flatMap {
        case Right(source) =>
          logger.info(s"Successfully downloaded archive - https://codeload.github.com$path")
          Future.successful(Some(source.runWith(StreamConverters.asInputStream())))
        case Left(UpstreamErrorResponse.WithStatusCode(404)) =>
          logger.info(s"https://codeload.github.com$path - not found")
          Future.successful(None)
        case Left(error) =>
          logger.error(s"Could not call https://codeload.github.com$path - ${error.getMessage}", error)
          throw error
      }
  }

  def streamGitHubAPI(path: String)(implicit hc: HeaderCarrier): Future[Option[InputStream]] = {
    logger.info(s"Getting code archive from: ${gitHubConfig.githubApiUrl}$path")
    httpClientV2
      .get(new URL(s"${gitHubConfig.githubApiUrl}$path"))
      .setHeader("Authorization" -> s"token ${gitHubConfig.githubToken}")
      .withProxy
      .stream[Either[UpstreamErrorResponse, Source[ByteString, _]]]
      .flatMap {
        case Right(source) =>
          logger.info(s"Successfully downloaded archive - ${gitHubConfig.githubApiUrl}$path")
          Future.successful(Some(source.runWith(StreamConverters.asInputStream())))
        case Left(UpstreamErrorResponse.WithStatusCode(404)) =>
          logger.info(s"${gitHubConfig.githubApiUrl}$path - not found")
          Future.successful(None)
        case Left(error) =>
          logger.error(s"Could not call ${gitHubConfig.githubApiUrl}$path - ${error.getMessage}", error)
          throw error
      }
  }
}
