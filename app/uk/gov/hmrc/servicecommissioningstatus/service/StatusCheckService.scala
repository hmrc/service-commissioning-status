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
import uk.gov.hmrc.servicecommissioningstatus.connectors.{ArtifactoryConnector, FrontendRoute, GitHubConnector, Release, ReleasesConnector, ServiceConfigsConnector}
import uk.gov.hmrc.servicecommissioningstatus.model.{AppConfigEnvironment, Dashboards, DeploymentEnvironment, Environment, FrontendRoutes, ServiceCommissioningStatus, StatusCheck}
import uk.gov.hmrc.servicecommissioningstatus.model.Environment._

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

  def commissioningStatusChecks(serviceName: String)(implicit hc: HeaderCarrier): Future[ServiceCommissioningStatus] = {
    for {
      repo <- checkRepoExists(serviceName)

      smConfig <- checkServiceManagerConfigExists(serviceName)

      frontend <- isFrontend(serviceName)
      routes   <- lookUpRoutes(serviceName)
      intRoute  = checkFrontendRoute(frontend, routes, Integration)
      devRoute  = checkFrontendRoute(frontend, routes, Development)
      qaRoute   = checkFrontendRoute(frontend, routes, QA)
      stagRoute = checkFrontendRoute(frontend, routes, Staging)
      etRoute   = checkFrontendRoute(frontend, routes, ExternalTest)
      prodRoute = checkFrontendRoute(frontend, routes, Production)

      appConfigBase <- checkAppConfigBaseExists(serviceName)

      appConfigInt  <- checkAppConfigExistsForEnv(serviceName, Integration)
      appConfigDev  <- checkAppConfigExistsForEnv(serviceName, Development)
      appConfigQA   <- checkAppConfigExistsForEnv(serviceName, QA)
      appConfigStag <- checkAppConfigExistsForEnv(serviceName, Staging)
      appConfigET   <- checkAppConfigExistsForEnv(serviceName, ExternalTest)
      appConfigProd <- checkAppConfigExistsForEnv(serviceName, Production)

      releases    <- releasesConnector.getReleases(serviceName).map(_.versions)
      intDeployed  = checkIsDeployedForEnv(serviceName, releases, Integration)
      devDeployed  = checkIsDeployedForEnv(serviceName, releases, Development)
      qaDeployed   = checkIsDeployedForEnv(serviceName, releases, QA)
      stagDeployed = checkIsDeployedForEnv(serviceName, releases, Staging)
      etDeployed   = checkIsDeployedForEnv(serviceName, releases, ExternalTest)
      prodDeployed = checkIsDeployedForEnv(serviceName, releases, Production)

      kibana    <- checkKibanaDashboardExists(serviceName)
      grafana   <- checkGrafanaDashboardExists(serviceName)

      buildJobs <- checkBuildJobsArchiveExists(serviceName)

      sensuZip   <- artifactoryConnector.getSensuZip
      alertConfig = checkAlertConfigExists(serviceName, sensuZip.get)

    } yield ServiceCommissioningStatus(
      serviceName = serviceName
      , hasRepo = repo
      , hasSMConfig = smConfig
      , hasFrontendRoutes = FrontendRoutes(intRoute, devRoute, qaRoute, stagRoute, etRoute, prodRoute)
      , hasAppConfigBase = appConfigBase
      , hasAppConfigEnv = AppConfigEnvironment(appConfigInt, appConfigDev, appConfigQA, appConfigStag, appConfigET, appConfigProd)
      , deployed = DeploymentEnvironment(intDeployed, devDeployed, qaDeployed, stagDeployed, etDeployed, prodDeployed)
      , hasDashboards = Dashboards(kibana, grafana)
      , hasBuildJobs = buildJobs
      , hasAlerts = alertConfig
    )
  }

  private def checkRepoExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    for {
      resp    <- gitHubConnector.getGithubRaw(s"/hmrc/$serviceName/main/repository.yaml")
      status  <- if (resp.isDefined) {
                    Future.successful(true)
                  } else {
                    gitHubConnector.getGithubApi(s"/repos/hmrc/$serviceName").map(_.isDefined)
                  }
      evidence = if (status) Some(s"https://github.com/hmrc/$serviceName") else None
    } yield StatusCheck(status, evidence)
  }


  private def checkServiceManagerConfigExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    val serviceManagerKey = serviceName.toUpperCase.replaceAll("[-]", "_")
    for {
      resp    <- gitHubConnector.getGithubRaw("/hmrc/service-manager-config/main/services.json")
      status   = resp.filter(_.contains(s"\"$serviceManagerKey\""))
      evidence = status.map(_ => s"https://github.com/hmrc/service-manager-config/blob/main/services.json")
    } yield StatusCheck(status.isDefined, evidence)
  }

  private def isFrontend(serviceName: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
     gitHubConnector
       .getGithubRaw(s"/hmrc/$serviceName/main/conf/application.conf")
       .map(_.exists(_.contains("\"frontend.conf\"")))
  }

  private def lookUpRoutes(serviceName: String)(implicit hc: HeaderCarrier): Future[Seq[FrontendRoute]] = {
    serviceConfigsConnector.getMDTPFrontendRoutes(serviceName)
  }

  private def checkFrontendRoute(isFrontend: Boolean, frontendRoutes: Seq[FrontendRoute], env: Environment): StatusCheck = {
    lazy val statusCheck = frontendRoutes
      .find(_.environment == env.asString)
      .map(maybeEnv => StatusCheck(status = true, Some(maybeEnv.routes.map(_.ruleConfigurationUrl).mkString)))
      .getOrElse(StatusCheck(status = false, Some(s"frontend with no routes in $env")))

    if (isFrontend) {
      statusCheck
    } else {
      StatusCheck(status = false, None)
    }
  }

  private def checkAppConfigBaseExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    for {
      resp    <- gitHubConnector.getGithubRaw(s"/hmrc/app-config-base/main/$serviceName.conf")
      status   = resp.isDefined
      evidence = resp.map(_ => s"https://github.com/hmrc/app-config-base/blob/main/$serviceName.conf")
    } yield StatusCheck(status, evidence)
  }

  private def checkAppConfigExistsForEnv(serviceName: String, environment: Environment)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    for {
      resp    <- gitHubConnector.getGithubRaw(s"/hmrc/app-config-${environment.asString}/main/$serviceName.yaml")
      status   = resp.isDefined
      evidence = resp.map(_ => s"https://github.com/hmrc/app-config-$environment/blob/main/$serviceName.yaml")
    } yield StatusCheck(status, evidence)
  }

  private def checkAlertConfigExists(serviceName: String, inputStream: InputStream): StatusCheck = {

    val githubLink = "https://github.com/hmrc/alert-config/tree/main/src/main/scala/uk/gov/hmrc/alertconfig/configs"
    val zip = new ZipInputStream(inputStream)

    Iterator.continually(zip.getNextEntry)
      .takeWhile(z => z != null)
      .foldLeft(StatusCheck(status = false, None))((found, entry) => {
        entry.getName match {
          case n if n.equals(s"target/output/configs/$serviceName.json") => StatusCheck(status = true, Some(githubLink))
          case _                                                         => found
        }
      })
  }

  private def checkIsDeployedForEnv(serviceName: String, releases: Seq[Release], env: Environment): StatusCheck = {
    if (releases.map(_.environment).contains(env.asString)) {
      StatusCheck(status = true, Some(s"https://catalogue.tax.service.gov.uk/deployment-timeline?service=$serviceName"))
    } else {
      StatusCheck(status = false, None)
    }
  }

  private def findInZip(inputStream: Option[InputStream], fileFilter: String)(extractor: PartialFunction[String, String]): Option[String] = {
    val zip = new ZipInputStream(inputStream.get)
    Iterator.continually(zip.getNextEntry)
      .takeWhile(_ != null)
      .filter(_.getName.contains(fileFilter))
      .map(_ => Source.fromInputStream(zip).getLines().mkString("\n"))
      .collectFirst(extractor)
  }

  private def checkKibanaDashboardExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    for {
      archive    <- gitHubConnector.streamGithubCodeLoad("/hmrc/kibana-dashboards/zip/refs/heads/main")
      maybeExists = findInZip(archive, "src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices"){
        case content if content.contains(s"Microservice(\"$serviceName\"") =>
          "https://github.com/hmrc/kibana-dashboards/tree/main/src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices"
      }
      status   = maybeExists.isDefined
      evidence = maybeExists.map(s => s)
    } yield StatusCheck(status, evidence)
  }

  private def checkGrafanaDashboardExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    for {
      archive    <- gitHubConnector.streamGithubCodeLoad("/hmrc/grafana-dashboards/zip/refs/heads/main")
      maybeExists = findInZip(archive, "src/main/scala/uk/gov/hmrc/grafanadashboards"){
        case content if content.contains(s"= \"$serviceName\"") =>
          "https://github.com/hmrc/grafana-dashboards/tree/main/src/main/scala/uk/gov/hmrc/grafanadashboards/dashboards"
      }
      status   = maybeExists.isDefined
      evidence = maybeExists.map(s => s)
    } yield StatusCheck(status, evidence)
  }

  private def checkBuildJobsArchiveExists(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    for {
      archive    <- gitHubConnector.streamGithubCodeLoad("/hmrc/build-jobs/zip/refs/heads/main")
      maybeExists = findInZip(archive, "jobs/live"){
        case content if content.contains(s"SbtMicroserviceJobBuilder(") && content.contains(s"\'$serviceName\'") =>
          "https://github.com/hmrc/build-jobs/tree/main/jobs/live"
      }
      status   = maybeExists.isDefined
      evidence = maybeExists.map(s => s)
    } yield StatusCheck(status, evidence)
  }
}
