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

import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.servicecommissioningstatus.connectors.{FrontendRoute, GitHubConnector, ServiceConfigsConnector}
import uk.gov.hmrc.servicecommissioningstatus.model.ServiceCommissioningStatus

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StatusCheckService @Inject()(
                                  gitHubConnector: GitHubConnector,
                                  serviceConfigsConnector: ServiceConfigsConnector
                                  )(implicit ec: ExecutionContext){

  // commissioningStatusChecks
  def commissioningStatusChecks(serviceName: String)(implicit hc: HeaderCarrier): Future[ServiceCommissioningStatus] = {
    for {
      repo <- repoStatus(serviceName)

      smConfig <- serviceManagerConfigStatus(serviceName)

      frontend  <- isFrontend(serviceName)
      routes    <- lookUpRoutes(serviceName)
      intRoute  = hasFrontendRoute(frontend, routes, "integration")
      devRoute  = hasFrontendRoute(frontend, routes, "development")
      qaRoute   = hasFrontendRoute(frontend, routes, "qa")
      stagRoute = hasFrontendRoute(frontend, routes, "staging")
      etRoute   = hasFrontendRoute(frontend, routes, "externaltest")
      prodRoute = hasFrontendRoute(frontend, routes, "production")

      appConfigInt  <- hasAppConfig(serviceName, "integration")
      appConfigDev  <- hasAppConfig(serviceName, "development")
      appConfigQA   <- hasAppConfig(serviceName, "qa")
      appConfigStag <- hasAppConfig(serviceName, "staging")
      appConfigET   <- hasAppConfig(serviceName, "externaltest")
      appConfigProd <- hasAppConfig(serviceName, "production")


    } yield ServiceCommissioningStatus(repo, smConfig, intRoute, devRoute, qaRoute, stagRoute, etRoute, prodRoute,
      appConfigInt, appConfigDev, appConfigQA, appConfigStag, appConfigET, appConfigProd)
  }

// Repo check or Repo Status
  private def repoStatus(serviceName: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    gitHubConnector.getRepository(serviceName).map(_.nonEmpty)
  }

  private def serviceManagerConfigStatus(serviceName: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    val serviceManagerKey = serviceName.toUpperCase.replaceAll("[-]", "_")
    // Adds quotes for regex exact match
    gitHubConnector.getServiceManagerConfigFile.map(_.exists(_.contains(s"\"$serviceManagerKey\"")))
  }

  private def isFrontend(serviceName: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    gitHubConnector.getApplicationConfigFile(serviceName).map(_.exists(_.contains("\"frontend.conf\"")))
  }

  private def lookUpRoutes(serviceName: String)(implicit hc: HeaderCarrier): Future[Seq[FrontendRoute]] = {
    serviceConfigsConnector.getMDTPFrontendRoutes(serviceName)
  }

  private def hasFrontendRoute(isFrontend: Boolean, routes: Seq[FrontendRoute], env: String)(implicit hc: HeaderCarrier): Boolean = {
    if (isFrontend) routes.map(_.environment).contains(env) else isFrontend
  }

  private def hasAppConfig(serviceName: String, environment: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    gitHubConnector.getAppConfigForEnvironment(serviceName, environment).map(_.nonEmpty)
  }

  private def hasBuildJobs(serviceName: String): Boolean = ???

  private def isDeployed(serviceName: String, env: String): Future[Boolean] = ???

  private def hasAlertConfig(serviceName: String): Boolean = ???

  private def hasKibanaDashboard(serviceName: String): Boolean = ???

  private def hasGrafanaDashboard(serviceName: String): Boolean = ???





}
