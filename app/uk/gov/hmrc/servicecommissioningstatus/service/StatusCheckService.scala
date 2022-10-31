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
import uk.gov.hmrc.servicecommissioningstatus.model.ServiceCommissioningStatus

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
  def commissioningStatusChecks(serviceName: String)(implicit hc: HeaderCarrier): Future[Seq[ServiceCommissioningStatus]] = {
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

      sensuZip    <- artifactoryConnector.getSensuZip
      alertConfig = hasAlertConfig(serviceName, sensuZip)

      releases      <- releasesConnector.getReleases(serviceName).map(_.versions)
      intDeployed   = isDeployed(releases, "integration")
      devDeployed   = isDeployed(releases, "development")
      qaDeployed    = isDeployed(releases, "qa")
      stagDeployed  = isDeployed(releases, "staging")
      etDeployed    = isDeployed(releases, "externaltest")
      prodDeployed  = isDeployed(releases, "production")

      kibanaArchive <- gitHubConnector.getArchive("kibana-dashboards")
      kibana = hasDashboard(serviceName, kibanaArchive)

      grafanaArchive <- gitHubConnector.getArchive("grafana-dashboards")
      grafana = hasDashboard(serviceName, grafanaArchive)


    } yield Seq(
        ServiceCommissioningStatus("hasRepo", repo)
      , ServiceCommissioningStatus("hasSMConfig", smConfig)
      , ServiceCommissioningStatus("hasIntegrationRoutes", intRoute)
      , ServiceCommissioningStatus("hasDevelopmentRoutes", devRoute)
      , ServiceCommissioningStatus("hasQARoutes", qaRoute)
      , ServiceCommissioningStatus("hasStagingRoutes", stagRoute)
      , ServiceCommissioningStatus("hasExternalTestRoutes", etRoute)
      , ServiceCommissioningStatus("hasProductionRoutes", prodRoute)
      , ServiceCommissioningStatus("hasIntegrationRoutes", intRoute)
      , ServiceCommissioningStatus("hasAppConfigIntegration", appConfigInt)
      , ServiceCommissioningStatus("hasAppConfigDevelopment", appConfigDev)
      , ServiceCommissioningStatus("hasAppConfigQA", appConfigQA)
      , ServiceCommissioningStatus("hasAppConfigStaging", appConfigStag)
      , ServiceCommissioningStatus("hasAppConfigExternalTest", appConfigET)
      , ServiceCommissioningStatus("hasAppConfigProduction", appConfigProd)
      , ServiceCommissioningStatus("hasAlertConfig", alertConfig)
      , ServiceCommissioningStatus("deployedInIntegration", intDeployed)
      , ServiceCommissioningStatus("deployedInDevelopment", devDeployed)
      , ServiceCommissioningStatus("deployedInQA", qaDeployed)
      , ServiceCommissioningStatus("deployedInStaging", stagDeployed)
      , ServiceCommissioningStatus("deployedInExternalTest", etDeployed)
      , ServiceCommissioningStatus("deployedInProduction", prodDeployed)
      , ServiceCommissioningStatus("hasKibanaDashboard", kibana)
      , ServiceCommissioningStatus("hasGrafanaDashboard", grafana)
    )
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

  private def hasBuildJobs(serviceName: String): Boolean = ???

  private def hasDashboard(serviceName: String, inputStream: Option[InputStream]) = {

    val zip = new ZipInputStream(inputStream.get)

    Iterator.continually(zip.getNextEntry)
      .takeWhile(z => z != null)
      .filter(f =>
        f.getName.contains("src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices") ||
        f.getName.contains("src/main/scala/uk/gov/hmrc/grafanadashboards"))
      .map { e =>
        e.getName -> Source.fromInputStream(zip).getLines.mkString("\n")
      }.foldLeft(false)((found, entry) => {
      entry match {
        //case (_, code)  if fileContent(code)  => {println("Found code for: " + serviceName); true}
        case (_, kibanaCode)  if kibanaCode.contains(s"Microservice(\"$serviceName\"")  => {println("Found code for: " + serviceName); true}
        case (_, grafanaCode) if grafanaCode.contains(s"= \"$serviceName\"")            => {println("Found code for: " + serviceName); true}
        case _                                                                          => found
      }
    })
  }








}
