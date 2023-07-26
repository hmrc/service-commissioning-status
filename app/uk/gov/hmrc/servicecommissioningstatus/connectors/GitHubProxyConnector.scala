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

import play.api.Logging
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class GitHubProxyConnector @Inject()(
  httpClientV2  : HttpClientV2,
  servicesConfig: ServicesConfig
)(implicit
  ec: ExecutionContext
) extends Logging {
  import HttpReads.Implicits._

  private lazy val gitHubProxyBaseURL = servicesConfig.baseUrl("platops-github-proxy")

  def getGitHubProxyRest(path: String)(implicit hc: HeaderCarrier): Future[Option[String]] =
    get(new URL(s"$gitHubProxyBaseURL/platops-github-proxy/github-rest$path"))

  def getGitHubProxyRaw(path: String)(implicit hc: HeaderCarrier): Future[Option[String]] =
    get(new URL(s"$gitHubProxyBaseURL/platops-github-proxy/github-raw$path"))

  private def get(url: URL)(implicit hc: HeaderCarrier): Future[Option[String]] =
    httpClientV2
      .get(url)
      .execute[HttpResponse]
      .flatMap( res =>
        res.status match {
          case 200 => Future.successful(Some(res.body))
          case 404 => Future.successful(None)
          case _   => Future.failed(new RuntimeException(s"Call to $url failed with ${res.status}, body: ${res.body}"))
        }
      )
}
