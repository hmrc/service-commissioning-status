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

import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, HttpResponse, StringContextOps}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.servicecommissioningstatus.config.GitHubConfig

import java.net.URL
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class GitHubConnector @Inject() (
  httpClientV2: HttpClientV2,
  gitHubConfig: GitHubConfig
)(implicit ec: ExecutionContext) {
  import HttpReads.Implicits._

  //TODO get head of file instead

  // Used to check Repo for service Exists
  // Refactor to only look for response
  def getRepository(serviceName: String)(implicit hc: HeaderCarrier): Future[Option[String]] = {
    val newHc = hc.withExtraHeaders(("Authorization", s"token ${gitHubConfig.githubToken}"))
    val requestUrl = url"${gitHubConfig.githubApiUrl}/repos/hmrc/$serviceName"
    doCall(requestUrl, newHc)
  }

  // Used to check service is in service manager config json file
  def getServiceManagerConfigFile(implicit hc: HeaderCarrier): Future[Option[String]] = {
    val newHc = hc.withExtraHeaders(("Authorization", s"token ${gitHubConfig.githubToken}"))
    val requestUrl = url"${gitHubConfig.githubRawUrl}/hmrc/service-manager-config/main/services.json"
    doCall(requestUrl, newHc)
  }

  // Used to check if service is a frontend-service
  def getApplicationConfigFile(serviceName: String)(implicit hc: HeaderCarrier): Future[Option[String]] = {
    val newHc = hc.withExtraHeaders(("Authorization", s"token ${gitHubConfig.githubToken}"))
    val requestUrl = url"${gitHubConfig.githubRawUrl}/hmrc/$serviceName/main/conf/application.conf"
    doCall(requestUrl, newHc)
  }

  //!TODO Change to get 200 Response instead of response.body
  //!TODO Create a trait for environments, see service-configs
  def getAppConfigForEnvironment(serviceName: String, environment: String)(implicit hc: HeaderCarrier): Future[Option[String]] = {
    val newHc = hc.withExtraHeaders(("Authorization", s"token ${gitHubConfig.githubToken}"))
    val requestUrl = url"${gitHubConfig.githubRawUrl}/hmrc/app-config-$environment/main/$serviceName.yaml"
    //println(">>>> " + requestUrl.toString + " <<<<")
    doCall(requestUrl, newHc)
  }

  private def doCall(url: URL, newHc: HeaderCarrier): Future[Option[String]] = {
    implicit val hc: HeaderCarrier = newHc
    httpClientV2
      .get(url)
      .withProxy
      .execute[HttpResponse]
      .map {
        case response if response.status == 200 =>
          //println(response.status.toString + " >>>> " + url + " <<<<")
          Some(response.body)
        case response if response.status == 404 =>
          //println(response.status.toString + " >>>> " + url + " <<<<")
          None
        case response =>
          sys.error(s"Failed with status code '${response.status}' to download GitHub file from $url")
      }
  }
}
