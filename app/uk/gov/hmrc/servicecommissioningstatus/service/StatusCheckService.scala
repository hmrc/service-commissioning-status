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
import uk.gov.hmrc.servicecommissioningstatus.model.{AppConfig, Dashboards, DeploymentEnvironment, FrontendRoutes, ServiceCommissioningStatus}

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

  // commissioningStatusChecks
  def commissioningStatusChecks(serviceName: String)(implicit hc: HeaderCarrier): Future[ServiceCommissioningStatus] = {
    for {
      repo <- repoStatus(serviceName)

      smConfig <- serviceManagerConfigStatus(serviceName)

      frontend <- isFrontend(serviceName)
      routes <- lookUpRoutes(serviceName)
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
      intDeployed = isDeployed(releases, "integration")
      devDeployed = isDeployed(releases, "development")
      qaDeployed = isDeployed(releases, "qa")
      stagDeployed = isDeployed(releases, "staging")
      etDeployed = isDeployed(releases, "externaltest")
      prodDeployed = isDeployed(releases, "production")

      kibanaArchive <- gitHubConnector.getArchive("kibana-dashboards")
      kibana = processArchive(serviceName, kibanaArchive)

      grafanaArchive <- gitHubConnector.getArchive("grafana-dashboards")
      grafana = processArchive(serviceName, grafanaArchive)

      buildJobsArchive <- gitHubConnector.getArchive("build-jobs")
      buildJobs = processArchive(serviceName, buildJobsArchive)

      sensuZip <- artifactoryConnector.getSensuZip
      alertConfig = hasAlertConfig(serviceName, sensuZip.get)

    } yield ServiceCommissioningStatus(
        repo
      , smConfig
      , FrontendRoutes(intRoute, devRoute, qaRoute, stagRoute, etRoute, prodRoute)
      , AppConfig(appConfigInt, appConfigDev, appConfigQA, appConfigStag, appConfigET, appConfigProd)
      , DeploymentEnvironment(intDeployed, devDeployed, qaDeployed, stagDeployed, etDeployed, prodDeployed)
      , Dashboards(kibana, grafana)
      , buildJobs
      , alertConfig
    )
  }

  private def repoStatus(serviceName: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    gitHubConnector.getRepository(serviceName).map(_.exists(_.status == 200))
  }

  private def serviceManagerConfigStatus(serviceName: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    val serviceManagerKey = serviceName.toUpperCase.replaceAll("[-]", "_")
    // Adds quotes for regex exact match
    gitHubConnector.getServiceManagerConfigFile.map(_.exists(_.body.contains(s"\"$serviceManagerKey\"")))
  }

  private def isFrontend(serviceName: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    gitHubConnector.getApplicationConfigFile(serviceName).map(_.exists(_.body.contains("\"frontend.conf\"")))
  }

  private def lookUpRoutes(serviceName: String): Future[Seq[FrontendRoute]] = {
    serviceConfigsConnector.getMDTPFrontendRoutes(serviceName)
  }

  private def hasFrontendRoute(isFrontend: Boolean, routes: Seq[FrontendRoute], env: String): Boolean = {
    if (isFrontend) routes.map(_.environment).contains(env) else isFrontend
  }

  private def hasAppConfig(serviceName: String, environment: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    gitHubConnector.getAppConfigForEnvironment(serviceName, environment).map(_.nonEmpty)
  }

  private def hasAlertConfig(serviceName: String, inputStream: InputStream): Boolean = {

    val zip = new ZipInputStream(inputStream)

    Iterator.continually(zip.getNextEntry)
      .takeWhile(z => z != null)
      .foldLeft(false)((found, entry) => {
        entry.getName match {
          case n if n.equals(s"target/output/configs/$serviceName.json") => true
          case _                                                         => found
        }
      })
  }

  private def isDeployed(releases: Seq[Release], env: String): Boolean = {
    releases.map(_.environment).contains(env)
  }

  private def processArchive(serviceName: String, inputStream: Option[InputStream]): Boolean = {

    val zip = new ZipInputStream(inputStream.get)

    Iterator.continually(zip.getNextEntry)
      .takeWhile(z => z != null)
      .filter(f =>
        f.getName.contains("src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices") ||
        f.getName.contains("src/main/scala/uk/gov/hmrc/grafanadashboards") ||
        f.getName.contains("jobs/live"))
      .map { e =>
        e.getName -> Source.fromInputStream(zip).getLines.mkString("\n")
      }.foldLeft(false)((found, entry) => {
      entry match {
        case (_, kibanaCode)    if kibanaCode.contains(s"Microservice(\"$serviceName\"") => {
          println("Found code for: " + serviceName); true}
        case (_, grafanaCode)   if grafanaCode.contains(s"= \"$serviceName\"")  => {
          println("Found code for: " + serviceName); true}
        case (_, buildJobCode)  if buildJobCode.contains(s"SbtMicroserviceJobBuilder(SERVICES") & buildJobCode.contains(s"\'$serviceName\'") => {
          println("Found code for: " + serviceName); true}
        case _ => found
      }
    })
  }








}
