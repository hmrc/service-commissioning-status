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
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, HttpResponse, UpstreamErrorResponse}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.servicecommissioningstatus.config.GitHubConfig

import java.io.InputStream
import java.net.URL
import javax.inject.Inject
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}


class GitHubConnector @Inject() (
  httpClientV2: HttpClientV2,
  gitHubConfig: GitHubConfig
)(implicit ec: ExecutionContext,
  materializer: Materializer) {
  import HttpReads.Implicits._


  def getGithubApi(path: String)(implicit hc: HeaderCarrier): Future[Option[HttpResponse]] = {
    val newHc = hc.withExtraHeaders(("Authorization", s"token ${gitHubConfig.githubToken}"))
    val requestUrl = new URL(s"${gitHubConfig.githubApiUrl}$path")
    doCall(requestUrl, newHc)
  }

  def getGithubRaw(path: String)(implicit hc: HeaderCarrier): Future[Option[HttpResponse]] = {
    val newHc = hc.withExtraHeaders(("Authorization", s"token ${gitHubConfig.githubToken}"))
    val requestUrl = new URL(s"${gitHubConfig.githubRawUrl}$path")
    doCall(requestUrl, newHc)
  }

  def streamGithubCodeLoad(path: String)(implicit hc: HeaderCarrier): Future[Option[InputStream]] = {
    httpClientV2
      .get(new URL(s"https://codeload.github.com$path"))
      .setHeader("Authorization" -> s"token ${gitHubConfig.githubToken}")
      .withProxy
      .stream[Either[UpstreamErrorResponse, Source[ByteString, _]]]
      .flatMap {
        case Right(source)                                   => Future.successful(Some(source.runWith(StreamConverters.asInputStream(readTimeout = 100000.seconds))))
        case Left(UpstreamErrorResponse.WithStatusCode(404)) => Future.successful(None)
        case Left(error)                                     => throw error
      }
  }

  def streamGitHubAPI(path: String)(implicit hc: HeaderCarrier): Future[Option[InputStream]] = {
    httpClientV2
      .get(new URL(s"${gitHubConfig.githubApiUrl}$path"))
      .setHeader("Authorization" -> s"token ${gitHubConfig.githubToken}")
      .withProxy
      .stream[Either[UpstreamErrorResponse, Source[ByteString, _]]]
      .flatMap {
        case Right(source)                                   => Future.successful(Some(source.runWith(StreamConverters.asInputStream(readTimeout = 100000.seconds))))
        case Left(UpstreamErrorResponse.WithStatusCode(404)) => Future.successful(None)
        case Left(error)                                     => throw error
      }
  }

  private def doCall(url: URL, newHc: HeaderCarrier): Future[Option[HttpResponse]] = {
    implicit val hc: HeaderCarrier = newHc
    httpClientV2
      .get(url)
      .withProxy
      .execute[HttpResponse]
      .map {
        case response if response.status == 200 =>
          Some(response)
        case response if response.status == 404 =>
          None
        case response =>
          sys.error(s"Failed with status code '${response.status}' to download GitHub file from $url")
      }
  }
}
