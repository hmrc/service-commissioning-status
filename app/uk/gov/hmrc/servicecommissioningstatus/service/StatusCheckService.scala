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

package uk.gov.hmrc.servicecommissioningstatus.service

import play.api.libs.json.Reads
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.servicecommissioningstatus.connectors.GitHubConnector
import uk.gov.hmrc.servicecommissioningstatus.model.ServiceCommissioningStatus

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StatusCheckService @Inject()(
                                  gitHubConnector: GitHubConnector
                                  )(implicit ec: ExecutionContext){

  // commissioningStatusChecks
  def commissioningChecks(serviceName: String)(implicit hc: HeaderCarrier): Future[ServiceCommissioningStatus] = {
    for {
      repoExists <- hasRepo(serviceName)
      inSMConfig <- hasServiceManagerConfig(serviceName)
    } yield ServiceCommissioningStatus(repoExists, inSMConfig)
  }


  private def hasRepo(serviceName: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    gitHubConnector.getRepository(serviceName).map(_.nonEmpty)
  }

  private def hasServiceManagerConfig(serviceName: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    val serviceManagerKey = serviceName.toUpperCase.replaceAll("[-]", "_")
    // Adds quotes for regex exact match
    gitHubConnector.getServiceManagerConfigFile.map(_.exists(_.contains(s"\"$serviceManagerKey\"")))
  }


  private def hasAppConfig(serviceName: String, env: String): Future[Boolean] = ???

  private def isFrontend(serviceName: String): Boolean = ???

  private def hasFrontendRoutes(serviceName: String): Boolean = ???

  private def hasBuildJobs(serviceName: String): Boolean = ???

  private def isDeployed(serviceName: String, env: String): Future[Boolean] = ???

  private def hasAlertConfig(serviceName: String): Boolean = ???

  private def hasKibanaDashboard(serviceName: String): Boolean = ???

  private def hasGrafanaDashboard(serviceName: String): Boolean = ???





}
