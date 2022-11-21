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

import java.io.InputStream
import java.util.zip.ZipInputStream
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source



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

      frontend    <- isFrontend(serviceName)
      routes      <- lookUpRoutes(serviceName)
      envRoutes    = FrontendRoutes(Environment.values.map(env => env -> checkFrontendRoute(frontend, routes, env)).toMap)

      appConfigBase <- checkAppConfigBaseExists(serviceName)

      appConfigEnvs <- Environment.values.foldLeftM(Map.empty[Environment, StatusCheck])((acc, env) =>
                        checkAppConfigExistsForEnv(serviceName, env)
                          .map(status => acc + (env -> status))
                        ).map(AppConfigEnvironment.apply)

      releases      <- releasesConnector.getReleases(serviceName).map(_.versions)
      deploymentEnv  = DeploymentEnvironment(Environment.values.map(env => env -> checkIsDeployedForEnv(serviceName, releases, env)).toMap)

      kibana      <- checkKibanaDashboardExists(serviceName)
      grafana     <- checkGrafanaDashboardExists(serviceName)
      buildJobs   <- checkBuildJobsArchiveExists(serviceName)
      alertConfig <- checkAlertConfigExists(serviceName)

    } yield ServiceCommissioningStatus(
      serviceName         = serviceName
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

  private def lookUpRoutes(serviceName: String)(implicit hc: HeaderCarrier): Future[Seq[FrontendRoute]] =
    serviceConfigsConnector.getMDTPFrontendRoutes(serviceName)

  private def checkFrontendRoute(isFrontend: Boolean, frontendRoutes: Seq[FrontendRoute], env: Environment): StatusCheck =
    if (isFrontend)
      frontendRoutes
      .find(_.environment == env.asString)
      .fold(StatusCheck(None))(fr => StatusCheck(Some(fr.routes.map(_.ruleConfigurationUrl).mkString)))
    else
      StatusCheck(None)

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

  private def checkAlertConfigExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    val githubLink  = "https://github.com/hmrc/alert-config/tree/main/src/main/scala/uk/gov/hmrc/alertconfig/configs"
    for {
      sensuZip   <- artifactoryConnector.getSensuZip
      optEvidence = findInZip(sensuZip, s"target/output/configs/$serviceName.json")(_ => githubLink)
    } yield  optEvidence.fold(StatusCheck(None))(evidence => StatusCheck(Some(evidence)))
  }

  private def checkIsDeployedForEnv(serviceName: String, releases: Seq[Release], env: Environment): StatusCheck =
    if (releases.map(_.environment).contains(env.asString))
      StatusCheck(Some(s"https://catalogue.tax.service.gov.uk/deployment-timeline?service=$serviceName"))
    else
      StatusCheck(None)

  private def findInZip(inputStream: Option[InputStream], fileFilter: String)(extractor: PartialFunction[String, String]): Option[String] =
   inputStream.flatMap { inputStream =>
      val zip = new ZipInputStream(inputStream)
      Iterator.continually(zip.getNextEntry)
        .takeWhile(_ != null)
        .filter(_.getName.contains(fileFilter))
        .map(_ => Source.fromInputStream(zip).getLines().mkString("\n"))
        .collectFirst(extractor)
    }

  private def checkKibanaDashboardExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] =
    for {
      archive <- gitHubConnector.streamGithubCodeload("/hmrc/kibana-dashboards/zip/refs/heads/main")
      optEvidence = findInZip(archive, "src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices") {
        case content if content.contains(s"Microservice(\"$serviceName\"") =>
          "https://github.com/hmrc/kibana-dashboards/tree/main/src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices"
      }
    } yield optEvidence.fold(StatusCheck(None))(evidence => StatusCheck(Some(evidence)))

  private def checkGrafanaDashboardExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] =
    for {
      archive    <- gitHubConnector.streamGithubCodeload("/hmrc/grafana-dashboards/zip/refs/heads/main")
      optEvidence = findInZip(archive, "src/main/scala/uk/gov/hmrc/grafanadashboards"){
        case content if content.contains(s"= \"$serviceName\"") =>
          "https://github.com/hmrc/grafana-dashboards/tree/main/src/main/scala/uk/gov/hmrc/grafanadashboards/dashboards"
      }
    } yield optEvidence.fold(StatusCheck(None))(evidence => StatusCheck(Some(evidence)))

  private def checkBuildJobsArchiveExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = 
    for {
      archive    <- gitHubConnector.streamGithubCodeload("/hmrc/build-jobs/zip/refs/heads/main")
      optEvidence = findInZip(archive, "jobs/live"){
        case content if content.contains(s"SbtMicroserviceJobBuilder(") && content.contains(s"\'$serviceName\'") =>
          "https://github.com/hmrc/build-jobs/tree/main/jobs/live"
      }
    } yield optEvidence.fold(StatusCheck(None))(evidence => StatusCheck(Some(evidence)))
}
