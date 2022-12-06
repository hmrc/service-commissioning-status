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
import uk.gov.hmrc.servicecommissioningstatus.connectors.{ArtifactoryConnector, FrontendRoute, GitHubConnector, Release, ReleasesConnector, ServiceConfigsConnector}
import uk.gov.hmrc.servicecommissioningstatus.model.{AppConfigEnvironment, Dashboard, DeploymentEnvironment, Environment, FrontendRoutes, ServiceCommissioningStatus, StatusCheck}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StatusCheckService @Inject()(
  gitHubConnector         : GitHubConnector,
  serviceConfigsConnector : ServiceConfigsConnector,
  artifactoryConnector    : ArtifactoryConnector,
  releasesConnector       : ReleasesConnector
)(implicit ec: ExecutionContext){

  def commissioningStatusChecks(serviceName: String)(implicit hc: HeaderCarrier): Future[ServiceCommissioningStatus] =
    for {
      repo <- checkRepoExists(serviceName)

      smConfig <- checkServiceManagerConfigExists(serviceName)

      isFrontend  <- isFrontend(serviceName)  // TODO check serviceType?
      routes      <- if (isFrontend) serviceConfigsConnector.getMDTPFrontendRoutes(serviceName)
                     else            Future.successful(Nil)
      envRoutes    = FrontendRoutes(Environment.values.map(env => env -> checkFrontendRoute(routes, env)).toMap) // TODO make optional

      appConfigBase <- checkAppConfigBaseExists(serviceName)

      appConfigEnvs <- Environment.values.foldLeftM(Map.empty[Environment, StatusCheck])((acc, env) =>
                         checkAppConfigExistsForEnv(serviceName, env)
                           .map(status => acc + (env -> status))
                       ).map(AppConfigEnvironment.apply)

      releases      <- releasesConnector.getReleases(serviceName).map(_.versions)
      deploymentEnv  = DeploymentEnvironment(Environment.values.map(env => env -> checkIsDeployedForEnv(serviceName, releases, env)).toMap)

      kibana      <- serviceConfigsConnector.getKibanaDashboard(serviceName)
      grafana     <- serviceConfigsConnector.getGrafanaDashboard(serviceName)
      buildJobs   <- serviceConfigsConnector.getBuildJobs(serviceName)
      alertConfig <- serviceConfigsConnector.getAlertConfig(serviceName)
    } yield ServiceCommissioningStatus(
      serviceName       = serviceName
    , hasRepo           = repo
    , hasSMConfig       = smConfig
    , hasFrontendRoutes = envRoutes
    , hasAppConfigBase  = appConfigBase
    , hasAppConfigEnv   = appConfigEnvs
    , isDeployed        = deploymentEnv
    , hasDashboards     = Dashboard(kibana, grafana)
    , hasBuildJobs      = buildJobs
    , hasAlerts         = alertConfig
    )

  private def checkRepoExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] =
    for {
      exists <- OptionT(gitHubConnector.getGithubRaw(s"/hmrc/$serviceName/main/repository.yaml"))
        .orElse(OptionT(gitHubConnector.getGithubApi(s"/repos/hmrc/$serviceName")))
        .value
    } yield
      if (exists.isDefined) StatusCheck(Some(s"https://github.com/hmrc/$serviceName"))
      else StatusCheck(None)


  private def checkServiceManagerConfigExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    val serviceManagerKey = serviceName.toUpperCase.replaceAll("[-]", "_")
    for {
      resp    <- gitHubConnector.getGithubRaw("/hmrc/service-manager-config/main/services.json")
      check    = resp.filter(_.contains(s"\"$serviceManagerKey\""))
      evidence = check.map(_ => s"https://github.com/hmrc/service-manager-config/blob/main/services.json")
    } yield StatusCheck(evidence)
  }

  private def isFrontend(serviceName: String)(implicit hc: HeaderCarrier): Future[Boolean] =
     gitHubConnector
       .getGithubRaw(s"/hmrc/$serviceName/main/conf/application.conf")
       .map(_.exists(_.contains("\"frontend.conf\"")))

  private def checkFrontendRoute(frontendRoutes: Seq[FrontendRoute], env: Environment): StatusCheck =
    frontendRoutes
      .find(_.environment == env.asString)
      .fold(StatusCheck(None))(fr => StatusCheck(fr.routes.map(_.ruleConfigurationUrl).headOption))

  private def checkAppConfigBaseExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] =
    for {
      resp    <- gitHubConnector.getGithubRaw(s"/hmrc/app-config-base/main/$serviceName.conf")
      evidence = resp.map(_ => s"https://github.com/hmrc/app-config-base/blob/main/$serviceName.conf")
    } yield StatusCheck(evidence)

  private def checkAppConfigExistsForEnv(serviceName: String, environment: Environment)(implicit hc: HeaderCarrier): Future[StatusCheck] =
    for {
      resp    <- gitHubConnector.getGithubRaw(s"/hmrc/app-config-${environment.asString}/main/$serviceName.yaml")
      evidence = resp.map(_ => s"https://github.com/hmrc/app-config-$environment/blob/main/$serviceName.yaml")
    } yield StatusCheck(evidence)

  private def checkIsDeployedForEnv(serviceName: String, releases: Seq[Release], env: Environment): StatusCheck =
    if (releases.map(_.environment).contains(env.asString))
      StatusCheck(Some(s"https://catalogue.tax.service.gov.uk/deployment-timeline?service=$serviceName"))
    else
      StatusCheck(None)
}
