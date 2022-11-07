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

import uk.gov.hmrc.http.{HeaderCarrier, StringContextOps}
import uk.gov.hmrc.servicecommissioningstatus.config.GitHubConfig
import uk.gov.hmrc.servicecommissioningstatus.connectors.{ArtifactoryConnector, FrontendRoute, GitHubConnector, Release, ReleasesConnector, ServiceConfigsConnector}
import uk.gov.hmrc.servicecommissioningstatus.model
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
  releasesConnector       : ReleasesConnector,
  gitHubConfig            : GitHubConfig
)(implicit ec: ExecutionContext){

  // commissioningStatusChecks
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
//
//      releases <- releasesConnector.getReleases(serviceName).map(_.versions)
//      intDeployed = isDeployed(releases, "integration")
//      devDeployed = isDeployed(releases, "development")
//      qaDeployed = isDeployed(releases, "qa")
//      stagDeployed = isDeployed(releases, "staging")
//      etDeployed = isDeployed(releases, "externaltest")
//      prodDeployed = isDeployed(releases, "production")
//
//      kibanaArchive <- gitHubConnector.streamGitHubAPI("kibana-dashboards")
//      kibana = processArchive(serviceName, kibanaArchive)
//
//      grafanaArchive <- gitHubConnector.streamGitHubAPI("grafana-dashboards")
//      grafana = processArchive(serviceName, grafanaArchive)
//
//      buildJobsArchive <- gitHubConnector.streamGitHubAPI("build-jobs")
//      buildJobs = processArchive(serviceName, buildJobsArchive)
//
//      sensuZip <- artifactoryConnector.getSensuZip
//      alertConfig = hasAlertConfig(serviceName, sensuZip.get)

    } yield ServiceCommissioningStatus(
        repo
      , smConfig
      , FrontendRoutes(intRoute, devRoute, qaRoute, stagRoute, etRoute, prodRoute)
      , AppConfig(appConfigInt, appConfigDev, appConfigQA, appConfigStag, appConfigET, appConfigProd)
//      , DeploymentEnvironment(intDeployed, devDeployed, qaDeployed, stagDeployed, etDeployed, prodDeployed)
//      , Dashboards(kibana, grafana)
//      , buildJobs
//      , alertConfig
    )
  }

//  private def repoStatus(serviceName: String)(implicit hc: HeaderCarrier): StatusCheck = {
//    //val statusCheck = gitHubConnector.getRepository(serviceName)._1.map(_.exists(_.status == 200))
//    val repo = gitHubConnector.getRepository(serviceName)
//    for {
//      repoExists: Boolean <- repo._1.map(_.exists(_.status == 200))
//      url: Option[String] <- if (repoExists) Some(repo._2) else None
//    } yield StatusCheck(repoExists, url)
//  }

  private def repoStatus(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    for {
      resp    <- gitHubConnector.getGithubApi(s"/repos/hmrc/$serviceName")
      status   = resp.filter(_.status == 200)
      evidence = status.map(_ => s"https://github.com/hmrc/$serviceName")
    } yield StatusCheck(status.nonEmpty, evidence)
  }

  private def serviceManagerConfigStatus(serviceName: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    val serviceManagerKey = serviceName.toUpperCase.replaceAll("[-]", "_")
    for {
      resp     <- gitHubConnector.getGithubRaw(s"/hmrc/service-manager-config/main/services.json")
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
      StatusCheck(status = false, Some("is backend, has no routes"))
    }
  }

  private def hasAppConfig(serviceName: String, environment: String)(implicit hc: HeaderCarrier): Future[StatusCheck] = {
    for {
      resp     <- gitHubConnector.getGithubRaw(s"/hmrc/app-config-$environment/main/$serviceName.yaml")
      status    = resp.filter(_.status == 200)
      evidence  = status.map(_ => s"https://github.com/hmrc/app-config-$environment/blob/main/$serviceName.yaml")
    } yield StatusCheck(status.nonEmpty, evidence)
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
          // https://github.com/hmrc/kibana-dashboards/tree/main/src/main/scala/uk/gov/hmrc/kibanadashboards/digitalservices
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
