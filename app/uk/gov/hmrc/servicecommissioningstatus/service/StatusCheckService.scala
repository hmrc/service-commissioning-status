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

import cats.data.OptionT
import cats.implicits._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.servicecommissioningstatus.connectors.{FrontendRoute, GitHubConnector, Release, ReleasesConnector, ServiceConfigsConnector}
import uk.gov.hmrc.servicecommissioningstatus.model.{Check, Environment}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StatusCheckService @Inject()(
  gitHubConnector         : GitHubConnector,
  serviceConfigsConnector : ServiceConfigsConnector,
  releasesConnector       : ReleasesConnector
)(implicit ec: ExecutionContext){

  import Check.{SimpleCheck, EnvCheck}
  def commissioningStatusChecks(serviceName: String)(implicit hc: HeaderCarrier): Future[List[Check]] =
    for {
      githubRepo      <- checkRepoExists(serviceName)
      appConfigBase   <- checkAppConfigBaseExists(serviceName)
      appConfigEnvs   <- Environment
                          .values
                          .foldLeftM(Map.empty[Environment, Check.Result]) {
                            (acc, env) => checkAppConfigExistsForEnv(serviceName, env).map(r => acc + (env -> r))
                          }
      isFrontend      <- isFrontend(serviceName)  // TODO check serviceType?
      oFrontendRoutes <- if (isFrontend)
                          serviceConfigsConnector
                            .getMDTPFrontendRoutes(serviceName)
                            .map(routes => Environment.values.map(env => env -> checkFrontendRouteForEnv(routes, env)).toMap)
                            .map(Some(_))
                         else Future.successful(None)
      buildJobs       <- serviceConfigsConnector.getBuildJobs(serviceName)
      smConfig        <- checkServiceManagerConfigExists(serviceName)
      deploymentEnv   <- releasesConnector
                          .getReleases(serviceName)
                          .map(releases => Environment.values.map(env => env -> checkIsDeployedForEnv(serviceName, releases.versions, env)).toMap)
      kibana          <- serviceConfigsConnector.getKibanaDashboard(serviceName)
      grafana         <- serviceConfigsConnector.getGrafanaDashboard(serviceName)
      alertConfig     <- serviceConfigsConnector.getAlertConfig(serviceName)
      checks           = SimpleCheck(title = "Github Repo"           , result  = githubRepo     ) ::
                         SimpleCheck(title = "App Config Base"       , result  = appConfigBase  ) ::
                         EnvCheck   (title = "App Config Environment", results = appConfigEnvs  ) ::
                         oFrontendRoutes.map(x => EnvCheck(title = "Frontend Routes", results = x )).toList :::
                         SimpleCheck(title = "Build Jobs"            , result  = buildJobs      ) ::
                         SimpleCheck(title = "Service Manager Config", result  = smConfig       ) ::
                         EnvCheck   (title = "Deployed"              , results = deploymentEnv  ) ::
                         SimpleCheck(title = "Kibana"                , result  = kibana         ) ::
                         SimpleCheck(title = "Grafana"               , result  = grafana        ) ::
                         SimpleCheck(title = "Alerts"                , result  = alertConfig    ) ::
                         Nil
    } yield checks

  private def checkRepoExists(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    OptionT(gitHubConnector.getGithubRaw(s"/hmrc/$serviceName/main/repository.yaml"))
      .orElse(OptionT(gitHubConnector.getGithubApi(s"/repos/hmrc/$serviceName")))
      .value
      .map {
        case Some(_) => Right(Check.Present(s"https://github.com/hmrc/$serviceName"))
        case None    => Left(Check.Missing("https://build.tax.service.gov.uk/job/PlatOps/job/Tools/job/create-a-repository/build"))
      }

  private def checkAppConfigBaseExists(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    gitHubConnector
      .getGithubRaw(s"/hmrc/app-config-base/main/$serviceName.conf")
      .map {
        case Some(_) => Right(Check.Present(s"https://github.com/hmrc/app-config-base/blob/main/$serviceName.conf"))
        case None    => Left(Check.Missing("https://github.com/hmrc/app-config-base"))
      }

  private def checkAppConfigExistsForEnv(serviceName: String, env: Environment)(implicit hc: HeaderCarrier): Future[Check.Result] =
    gitHubConnector
      .getGithubRaw(s"/hmrc/app-config-${env.asString}/main/$serviceName.yaml")
      .map {
        case Some(_) => Right(Check.Present(s"https://github.com/hmrc/app-config-${env.asString}/blob/main/$serviceName.yaml"))
        case None    => Left(Check.Missing(s"https://github.com/hmrc/app-config-${env.asString}"))
      }

  private def isFrontend(serviceName: String)(implicit hc: HeaderCarrier): Future[Boolean] =
     gitHubConnector
       .getGithubRaw(s"/hmrc/$serviceName/main/conf/application.conf")
       .map(_.exists(_.contains("\"frontend.conf\"")))

  private def checkFrontendRouteForEnv(frontendRoutes: Seq[FrontendRoute], env: Environment): Check.Result =
    frontendRoutes
      .find(_.environment == env.asString)
      .flatMap(_.routes.map(_.ruleConfigurationUrl).headOption) match {
        case Some(e) => Right(Check.Present(e))
        case None    => Left(Check.Missing(s"https://github.com/hmrc/mdtp-frontend-routes/${env.asString}"))
      }

  private def checkServiceManagerConfigExists(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    for {
      optStr  <- gitHubConnector.getGithubRaw("/hmrc/service-manager-config/main/services.json")
      key      = serviceName.toUpperCase.replaceAll("[-]", "_")
      evidence = optStr
                  .getOrElse("")
                  .linesIterator
                  .zipWithIndex
                  .find { case (line, _) => line.contains(s"\"$key\"") }
                  .map { case (_, idx) => s"https://github.com/hmrc/service-manager-config/blob/main/services.json#L${idx + 1}" }
    } yield evidence match {
      case Some(e) => Right(Check.Present(e))
      case None    => Left(Check.Missing(s"https://github.com/hmrc/service-manager-config"))
    }

  private def checkIsDeployedForEnv(serviceName: String, releases: Seq[Release], env: Environment): Check.Result =
    if (releases.map(_.environment).contains(env.asString))
      Right(Check.Present(s"https://catalogue.tax.service.gov.uk/deployment-timeline?service=$serviceName"))
    else
      Left(Check.Missing(s"https://orchestrator.tools.${env.asString}.tax.service.gov.uk/job/deploy-microservice"))
}
