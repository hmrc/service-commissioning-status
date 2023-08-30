/*
 * Copyright 2023 HM Revenue & Customs
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
import play.api.Configuration
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.servicecommissioningstatus.connectors.ServiceMetricsConnector.MongoCollectionSize
import uk.gov.hmrc.servicecommissioningstatus.connectors.TeamsAndRepositoriesConnector.BuildJobType
import uk.gov.hmrc.servicecommissioningstatus.connectors._
import uk.gov.hmrc.servicecommissioningstatus.connectors.model.InternalAuthConfig
import uk.gov.hmrc.servicecommissioningstatus.model.Check.{Missing, Present}
import uk.gov.hmrc.servicecommissioningstatus.model.Environment.{Production, QA}
import uk.gov.hmrc.servicecommissioningstatus.model.{Check, Environment}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StatusCheckService @Inject()(
  config                  : Configuration,
  gitHubProxyConnector    : GitHubProxyConnector,
  serviceConfigsConnector : ServiceConfigsConnector,
  releasesConnector       : ReleasesConnector,
  teamsAndReposConnector  : TeamsAndRepositoriesConnector,
  serviceMetricsConnector : ServiceMetricsConnector,
)(implicit ec: ExecutionContext){

  import scala.jdk.CollectionConverters._

  private val environmentsToHideWhenUnconfigured: Set[Environment] =
    config.underlying.getStringList("environmentsToHideWhenUnconfigured").asScala.toSet.map { str: String =>
      Environment.parse(str).getOrElse(sys.error(s"config 'environmentsToHideWhenUnconfigured' contains an invalid environment: $str"))
    }

  import Check.{EnvCheck, SimpleCheck}


  def commissioningStatusChecks(serviceName: String)(implicit hc: HeaderCarrier): Future[List[Check]] =
    for {
      oRepo           <- teamsAndReposConnector.findRepo(serviceName)
      isFrontend      =  oRepo.flatMap(_.serviceType).contains(TeamsAndRepositoriesConnector.ServiceType.Frontend)
      isAdminFrontend =  oRepo.map(_.tags).exists(_.contains(TeamsAndRepositoriesConnector.Tag.AdminFrontend))

      githubRepo      <- checkRepoExists(serviceName)
      appConfigBase   <- checkAppConfigBaseExists(serviceName)
      appConfigEnvs   <- Environment
                          .values
                          .foldLeftM(Map.empty[Environment, Check.Result]) {
                            (acc, env) => checkAppConfigExistsForEnv(serviceName, env).map(r => acc + (env -> r))
                          }
      oMdptFrontend   <- serviceConfigsConnector
                          .getMDTPFrontendRoutes(serviceName)
                          .map(routes => Environment.values.map(env => env -> checkFrontendRouteForEnv(routes, env)).toMap)
                          .map(xs => Option.when(xs.values.exists(_.isRight) || (isFrontend && !isAdminFrontend))(xs))
      oAdminFrontend  <- serviceConfigsConnector
                          .getAdminFrontendRoutes(serviceName)
                          .map(routes => Environment.values.map(env => env -> checkAdminFrontendRouteForEnv(routes, env)).toMap)
                          .map(xs => Option.when(xs.values.exists(_.isRight) || isAdminFrontend)(xs))
      oInternalAuthConfig <- serviceConfigsConnector
                          .getInternalAuthConfig(serviceName)
                          .map{ configs =>
                            Map[Environment, Check.Result](
                              QA         -> checkForInternalAuthEnvironment(configs,"qa", QA),
                              Production -> checkForInternalAuthEnvironment(configs, "prod", Production))}
                          .map(xs => Option.when(xs.values.exists(_.isRight))(xs))
      buildJobs       <- serviceConfigsConnector.getBuildJobs(serviceName)
      pipelineJob     <- checkPipelineJob(serviceName)
      smConfig        <- checkServiceManagerConfigExists(serviceName)
      kibana          <- serviceConfigsConnector.getKibanaDashboard(serviceName)
      grafana         <- serviceConfigsConnector.getGrafanaDashboard(serviceName)
      alertConfig     <- serviceConfigsConnector.getAlertConfig(serviceName)
      deploymentEnv   <- releasesConnector
                          .getReleases(serviceName)
                          .map(releases => Environment.values.map(env => env -> checkIsDeployedForEnv(serviceName, releases.versions, env)).toMap)
      mongoDb         <- serviceMetricsConnector
                           .getCollections(serviceName)
                           .map(mcss => Environment.values.map(env => env -> checkMongoDbExistsInEnv(mcss, env)).toMap)
      shutterPageEnvs <- serviceConfigsConnector.getShutterPages(serviceName)
      allChecks        = SimpleCheck(
                           title      = "Github Repo",
                           result     = githubRepo,
                           helpText   = "Has the Github repository for the service been created? This is where the source code for the service is stored and managed.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-github-repository.html")
                         ) ::
                         SimpleCheck(
                           title      = "App Config Base",
                           result     = appConfigBase,
                           helpText   = "Additional configuration included in the build and applied to all environments.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-app-config.html")
                         ) ::
                         EnvCheck(
                           title      = "App Config Environment",
                           results    = appConfigEnvs,
                           helpText   = "Environment specific configuration.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-app-config.html")
                         ) ::
                         oInternalAuthConfig.map { results =>
                           EnvCheck(
                              title    = "Internal Auth Configs",
                              results  = results,
                              helpText = "Indicates if a service is a Grantee or Grantor in Internal Auth",
                              linkToDocs = None
                           )
                         }.toList :::
                         oMdptFrontend.map { results =>
                           EnvCheck(
                             title      = "Frontend Routes",
                             results    = results,
                             helpText   = "Configuration required to expose the service under the tax.service.gov.uk domain.",
                             linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/allow-user-access.html")
                           )
                         }.toList :::
                         oAdminFrontend.map { results =>
                           EnvCheck(
                             title      = "Admin Frontend Routes",
                             results    = results,
                             helpText   = "Configuration required to expose the service under the admin.tax.service.gov.uk domain for internal admin users.",
                             linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/allow-user-access.html")
                           )
                         }.toList :::
                         SimpleCheck(
                           title      = "Build Jobs",
                           result     = buildJobs,
                           helpText   = "Configuration required to trigger test runs or deploy to pre-production environments automatically on merge.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-a-ci-cd-pipeline.html")
                         ) ::
                         SimpleCheck(
                           title      = "Pipeline Jobs",
                           result     = pipelineJob,
                           helpText   = "Configuration to automatically deploy to lower environments when a build completes",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-a-ci-cd-pipeline.html#add-the-pipelinejobbuilder")
                         ) ::
                         SimpleCheck(
                           title = "Service Manager Config",
                           result  = smConfig,
                           helpText = "Allows the service to be run with service-manager.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/add-a-microservice-to-service-manager.html")
                         ) ::
                         SimpleCheck(
                           title      = "Logging - Kibana",
                           result     = kibana,
                           helpText   = "Creates a convenient dashboard in Kibana for the service.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/add-a-logs-dashboard-using-Kibana.html")
                         ) ::
                         SimpleCheck(
                           title = "Metrics - Grafana",
                           result  = grafana,
                           helpText = "Creates a dashboard in Grafana for viewing the service's metrics.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/add-a-metrics-dashboard-using-Grafana.html")
                         ) ::
                         SimpleCheck(
                           title = "Alerts - PagerDuty",
                           result  = alertConfig,
                           helpText = "Enables and configures PagerDuty alerts for the service.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/add-service-alerting-using-pagerduty.html")
                         ) ::
                         EnvCheck(
                           title = "Deployed",
                           results = deploymentEnv,
                           helpText = "Which environments the service has been deployed to.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/carry-out-an-early-deployment-to-production.html")
                         ) ::
                         EnvCheck(
                           title = "Mongo Database",
                           results = mongoDb,
                           helpText = "Which environments have stored data in Mongo.",
                           linkToDocs = None
                         ) ::
                         EnvCheck(
                           title = "Custom Shutter Pages",
                           results = shutterPageEnvs.toMap,
                           helpText = "Which environments have custom shutter pages. These are optional.",
                           linkToDocs = Some("https://confluence.tools.tax.service.gov.uk/display/DTRG/Shuttering+your+service#Shutteringyourservice-Configuringshutteringformyservice")
                         ) ::
                         Nil
      checks           = StatusCheckService.hideUnconfiguredEnvironments(allChecks, environmentsToHideWhenUnconfigured)
    } yield checks

  private def checkRepoExists(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    OptionT(teamsAndReposConnector.findRepo(serviceName))
      .value
      .map {
        case Some(repo) if !repo.isArchived => Right(Check.Present(repo.githubUrl))
        case _ => Left(Check.Missing("https://build.tax.service.gov.uk/job/PlatOps/job/Tools/job/create-a-repository/build"))
      }

  private def checkAppConfigBaseExists(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    gitHubProxyConnector
      .getGitHubProxyRaw(s"/app-config-base/main/$serviceName.conf")
      .map {
        case Some(_) => Right(Check.Present(s"https://github.com/hmrc/app-config-base/blob/main/$serviceName.conf"))
        case None    => Left(Check.Missing("https://build.tax.service.gov.uk/job/PlatOps/job/Tools/job/create-app-configs/build"))
      }

  private def checkAppConfigExistsForEnv(serviceName: String, env: Environment)(implicit hc: HeaderCarrier): Future[Check.Result] =
    gitHubProxyConnector
      .getGitHubProxyRaw(s"/app-config-${env.asString}/main/$serviceName.yaml")
      .map {
        case Some(_) => Right(Check.Present(s"https://github.com/hmrc/app-config-${env.asString}/blob/main/$serviceName.yaml"))
        case None    => Left(Check.Missing("https://build.tax.service.gov.uk/job/PlatOps/job/Tools/job/create-app-configs/build"))
      }

  private def checkFrontendRouteForEnv(frontendRoutes: Seq[ServiceConfigsConnector.FrontendRoute], env: Environment): Check.Result =
    frontendRoutes
      .find(_.environment == env)
      .flatMap(_.routes.map(_.ruleConfigurationUrl).headOption) match {
        case Some(e) => Right(Check.Present(e))
        case None    => Left(Check.Missing(s"https://github.com/hmrc/mdtp-frontend-routes/tree/main/${env.asString}"))
      }

  private def checkAdminFrontendRouteForEnv(adminFrontendRoutes: Seq[ServiceConfigsConnector.AdminFrontendRoute], env: Environment): Check.Result =
    adminFrontendRoutes.headOption match {
      case Some(e)
        if e.allow.contains(env)
             => Right(Check.Present(e.location))
      case _ => Left(Check.Missing(s"https://github.com/hmrc/admin-frontend-proxy"))
    }

  private def checkForInternalAuthEnvironment(configs: Seq[InternalAuthConfig], internalAuthEnv: String, environment: Environment): Check.Result = {
      val url = s"https://github.com/hmrc/internal-auth-config/tree/main/$internalAuthEnv"
      if (configs.exists(cfg => cfg.environment == environment)) {
        Right(Present(url))
      } else {
        Left(Missing(url))
      }
  }

  private def checkPipelineJob(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    for {
      jobs <- teamsAndReposConnector.findBuildJobs(serviceName)
      check = jobs.find(_.jobType == BuildJobType.Pipeline)
    } yield check match {
      case Some(job) => Right(Check.Present(job.jenkinsUrl))
      case None      => Left(Check.Missing(s"https://github.com/hmrc/build-jobs"))
    }

  private def checkServiceManagerConfigExists(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    for {
      optStr  <- gitHubProxyConnector.getGitHubProxyRaw("/service-manager-config/main/services.json")
      key      = serviceName.toUpperCase.replaceAll("-", "_")
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

  private def checkIsDeployedForEnv(serviceName: String, releases: Seq[ReleasesConnector.Release], env: Environment): Check.Result =
    if (releases.map(_.environment).contains(env.asString))
      Right(Check.Present(s"https://catalogue.tax.service.gov.uk/deployment-timeline?service=$serviceName"))
    else
      Left(Check.Missing(s"https://build.tax.service.gov.uk/job/build-and-deploy/job/deploy-microservice/build?delay=0sec"))

  private def checkMongoDbExistsInEnv(collections: Seq[MongoCollectionSize], env: Environment): Check.Result =
    if (collections.map(_.environment).contains(env)) {
      val db = collections.headOption.fold("")(_.database)
      Right(Check.Present(s"https://grafana.tools.${env.asString}.tax.service.gov.uk/d/platops-mongo-collections?var-replica_set=*&var-database=$db&var-collection=All&orgId=1"))
    } else Left(Check.Missing(""))
}

object StatusCheckService {

  import Check.{SimpleCheck, EnvCheck}
  def hideUnconfiguredEnvironments(checks: List[Check], environments: Set[Environment]): List[Check] = {
    val configured =
      checks
        .flatMap {
          case _: SimpleCheck => Nil
          case x: EnvCheck    => x.results.toList
        }
        .collect { case (k, v) if environments.contains(k) && v.isRight => k }
        .toSet

    checks.collect {
      case x: SimpleCheck => x
      case x: EnvCheck    => x.copy(results = x.results.removedAll(environments.removedAll(configured)))
    }
  }
}
