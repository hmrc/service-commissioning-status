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
import uk.gov.hmrc.servicecommissioningstatus.model.{AppConfig, Dashboards, DeploymentEnvironment, FrontendRoutes, ServiceCommissioningStatus, StatusCheck}

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
      repo <- repoStatus(serviceName)

      smConfig <- serviceManagerConfigStatus(serviceName)

      frontend <- isFrontend(serviceName)
      routes   <- lookUpRoutes(serviceName)
      intRoute = hasFrontendRoute(frontend, routes, "integration")
      devRoute = hasFrontendRoute(frontend, routes, "development")
      qaRoute = hasFrontendRoute(frontend, routes, "qa")
      stagRoute = hasFrontendRoute(frontend, routes, "staging")
      etRoute = hasFrontendRoute(frontend, routes, "externaltest")
      prodRoute = hasFrontendRoute(frontend, routes, "production")

      appConfigInt <- hasAppConfig(serviceName, "integration")
      appConfigDev <- hasAppConfig(serviceName, "development")
      appConfigQA <- hasAppConfig(serviceName, "qa")
      appConfigStag <- hasAppConfig(serviceName, "staging")
      appConfigET <- hasAppConfig(serviceName, "externaltest")
      appConfigProd <- hasAppConfig(serviceName, "production")

      releases <- releasesConnector.getReleases(serviceName).map(_.versions)
      intDeployed = isDeployed(serviceName, releases, "integration")
      devDeployed = isDeployed(serviceName, releases, "development")
      qaDeployed = isDeployed(serviceName, releases, "qa")
      stagDeployed = isDeployed(serviceName, releases, "staging")
      etDeployed = isDeployed(serviceName, releases, "externaltest")
      prodDeployed = isDeployed(serviceName, releases, "production")

      kibanaArchive <- gitHubConnector.streamGithubCodeLoad("/hmrc/kibana-dashboards/zip/refs/heads/main")
      kibana = processArchive(serviceName, kibanaArchive)

      grafanaArchive <- gitHubConnector.streamGithubCodeLoad("/hmrc/grafana-dashboards/zip/refs/heads/main")
      grafana = processArchive(serviceName, grafanaArchive)

      buildJobsArchive <- gitHubConnector.streamGithubCodeLoad("/hmrc/build-jobs/zip/refs/heads/main")
      buildJobs = processArchive(serviceName, buildJobsArchive)

      sensuZip <- artifactoryConnector.getSensuZip
      alertConfig = hasAlertConfig(serviceName, sensuZip.get)

    } yield ServiceCommissioningStatus(
      serviceName
      , repo
      , smConfig
      , FrontendRoutes(intRoute, devRoute, qaRoute, stagRoute, etRoute, prodRoute)
      , AppConfig(appConfigInt, appConfigDev, appConfigQA, appConfigStag, appConfigET, appConfigProd)
      , DeploymentEnvironment(intDeployed, devDeployed, qaDeployed, stagDeployed, etDeployed, prodDeployed)
      , Dashboards(kibana, grafana)
      , buildJobs
      , alertConfig
    )
  }

  private def repoStatus(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {

    for {
      resp    <- gitHubConnector.getGithubRaw(s"/hmrc/$serviceName/main/repository.yaml")
      status  <- if (resp.exists(_.status != 404)) { Future.successful(true)
      } else {
        gitHubConnector.getGithubApi(s"/repos/hmrc/$serviceName").map(_.exists(_.status == 200))
      }
      evidence = if (status) Some(s"https://github.com/hmrc/$serviceName") else None
    } yield StatusCheck(status, evidence)
  }

  private def serviceManagerConfigStatus(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    val serviceManagerKey = serviceName.toUpperCase.replaceAll("[-]", "_")
    for {
      resp     <- gitHubConnector.getGithubRaw("/hmrc/service-manager-config/main/services.json")
      status    = resp.filter(_.body.contains(s"\"$serviceManagerKey\""))
      evidence  = status.map(_ => s"https://github.com/hmrc/service-manager-config/blob/main/services.json")
    } yield StatusCheck(status.nonEmpty, evidence)

  }

  private def isFrontend(serviceName: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
     gitHubConnector
       .getGithubRaw(s"/hmrc/$serviceName/main/conf/application.conf")
       .map(_.exists(_.body.contains("\"frontend.conf\"")))
  }

  private def lookUpRoutes(serviceName: String): Future[Seq[FrontendRoute]] = {
    serviceConfigsConnector.getMDTPFrontendRoutes(serviceName)
  }

  private def hasFrontendRoute(isFrontend: Boolean, frontendRoutes: Seq[FrontendRoute], env: String): StatusCheck= {
    lazy val statusCheck = frontendRoutes
      .find(_.environment == env)
      .map(maybeEnv => StatusCheck(status = true, Some(maybeEnv.routes.map(_.ruleConfigurationUrl).mkString)))
      .getOrElse(StatusCheck(status = false, Some(s"frontend with no routes in $env")))

    if (isFrontend) {
      statusCheck
    } else {
      StatusCheck(status = false, None)
    }
  }

  private def hasAppConfig(serviceName: String, environment: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    for {
      resp     <- gitHubConnector.getGithubRaw(s"/hmrc/app-config-$environment/main/$serviceName.yaml")
      status    = resp.filter(_.status == 200)
      evidence  = status.map(_ => s"https://github.com/hmrc/app-config-$environment/blob/main/$serviceName.yaml")
    } yield StatusCheck(status.nonEmpty, evidence)
  }

  private def hasAlertConfig(serviceName: String, inputStream: InputStream): StatusCheck = {

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

  private def isDeployed(serviceName: String, releases: Seq[Release], env: String): StatusCheck = {
    if (releases.map(_.environment).contains(env)) {
      StatusCheck(status = true, Some(s"https://catalogue.tax.service.gov.uk/deployment-timeline?service=$serviceName"))
    } else {
      StatusCheck(status = false, None)
    }
  }

  private def processArchive(serviceName: String, inputStream: Option[InputStream]): StatusCheck = {

    val zip = new ZipInputStream(inputStream.get)

    Iterator.continually(zip.getNextEntry)
      .takeWhile(z => z != null)
      .filter(f =>
        f.getName.contains("src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices") ||
        f.getName.contains("src/main/scala/uk/gov/hmrc/grafanadashboards")                ||
        f.getName.contains("jobs/live"))
      .map { e =>
        e.getName -> Source.fromInputStream(zip).getLines.mkString("\n")
      }.foldLeft(StatusCheck(status = false, None))((found, entry) => {
      entry match {
        case (_, kibanaCode)    if kibanaCode.contains(s"Microservice(\"$serviceName\"") =>
          StatusCheck(status = true, Some("https://github.com/hmrc/kibana-dashboards/tree/main/src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices"))
        case (_, grafanaCode)   if grafanaCode.contains(s"= \"$serviceName\"")           =>
          StatusCheck(status = true, Some("https://github.com/hmrc/grafana-dashboards/tree/main/src/main/scala/uk/gov/hmrc/grafanadashboards/dashboards"))
        case (_, buildJobCode)  if buildJobCode.contains(s"SbtMicroserviceJobBuilder(SERVICES")
          & buildJobCode.contains(s"\'$serviceName\'")                                   =>
          StatusCheck(status = true, Some("https://github.com/hmrc/build-jobs/tree/main/jobs/live"))
        case _                                                                           => found
      }
    })
  }
}
