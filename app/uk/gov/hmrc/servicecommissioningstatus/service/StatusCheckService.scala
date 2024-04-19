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

import cats.implicits._
import play.api.Configuration
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.servicecommissioningstatus.{Check, Environment, LifecycleStatus, ServiceName, ServiceType, TeamName}
import uk.gov.hmrc.servicecommissioningstatus.connectors._
import uk.gov.hmrc.servicecommissioningstatus.persistence.{CacheRepository, LifecycleStatusRepository}
import uk.gov.hmrc.servicecommissioningstatus.persistence.LifecycleStatusRepository.Lifecycle

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StatusCheckService @Inject()(
  config                     : Configuration
, serviceConfigsConnector    : ServiceConfigsConnector
, releasesConnector          : ReleasesConnector
, teamsAndReposConnector     : TeamsAndRepositoriesConnector
, serviceMetricsConnector    : ServiceMetricsConnector
, slackNotificationsConnector: SlackNotificationsConnector
, cachedRepository           : CacheRepository
, lifecycleStatusRepository  : LifecycleStatusRepository
){

  // TODO get from service commissioning status?
  def listAllChecks(): List[(String, Class[_ <: Check])] =
    ("Github Repo"             -> classOf[Check.SimpleCheck]) ::
    ("App Config Base"         -> classOf[Check.SimpleCheck]) ::
    ("App Config Environment"  -> classOf[Check.EnvCheck   ]) ::
    ("Internal Auth Configs"   -> classOf[Check.EnvCheck   ]) ::
    ("Frontend Routes"         -> classOf[Check.EnvCheck   ]) ::
    ("Admin Frontend Routes"   -> classOf[Check.EnvCheck   ]) ::
    ("Build Jobs"              -> classOf[Check.SimpleCheck]) ::
    ("Pipeline Jobs"           -> classOf[Check.SimpleCheck]) ::
    ("Service Manager Config"  -> classOf[Check.SimpleCheck]) ::
    ("Logging - Kibana"        -> classOf[Check.SimpleCheck]) ::
    ("Metrics - Grafana"       -> classOf[Check.SimpleCheck]) ::
    ("Alerts - PagerDuty"      -> classOf[Check.SimpleCheck]) ::
    ("Deployed"                -> classOf[Check.EnvCheck   ]) ::
    ("Mongo Database"          -> classOf[Check.EnvCheck   ]) ::
    ("Custom Shutter Pages"    -> classOf[Check.EnvCheck   ]) ::
    ("Upscan Config"           -> classOf[Check.EnvCheck   ]) ::
    Nil

  def updateCache()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Int] =
    for {
      services <- ( teamsAndReposConnector.findServiceRepos()
                  , teamsAndReposConnector.findDeletedServiceRepos()
                  ).mapN(_ ++ _)
                   .map(_.sortBy(_.name))
      results  <- services.foldLeftM[Future, Seq[CacheRepository.ServiceCheck]](List.empty) { (acc, repo) =>
                    for {
                      lifecycle <- lifecycleStatus(repo)
                      checks    <- commissioningStatusChecks(ServiceName(repo.name))
                    } yield acc :+ CacheRepository.ServiceCheck(ServiceName(repo.name), lifecycle.lifecycleStatus, checks)
                  }
      _        <- cachedRepository.putAll(results)
    } yield results.size

  def cachedCommissioningStatusChecks(
    teamName       : Option[TeamName],
    serviceType    : Option[ServiceType],
    lifecycleStatus: List[LifecycleStatus]
  )(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[Seq[CacheRepository.ServiceCheck]] =
    for {
      services  <- ( teamsAndReposConnector.findServiceRepos(team = teamName, serviceType = serviceType)
                   , teamsAndReposConnector.findDeletedServiceRepos(team = teamName, serviceType = serviceType)
                   ).mapN(_ ++ _)
                    .map(_.sortBy(_.name))
      results   <- cachedRepository.findAll(services.map(repo => ServiceName(repo.name)), lifecycleStatus)
    } yield results

  private lazy val environmentsToHideWhenUnconfigured: Set[Environment] = {
    import scala.jdk.CollectionConverters._
    config.underlying.getStringList("environmentsToHideWhenUnconfigured").asScala.toSet.map { str: String =>
      Environment.parse(str).getOrElse(sys.error(s"config 'environmentsToHideWhenUnconfigured' contains an invalid environment: $str"))
    }
  }

  import Check.{EnvCheck, SimpleCheck}
  def commissioningStatusChecks(serviceName: ServiceName)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[List[Check]] =
    for {
      oRepo           <- ( teamsAndReposConnector.findServiceRepos(serviceName = Some(serviceName))
                         , teamsAndReposConnector.findDeletedServiceRepos(serviceName = Some(serviceName))
                         ).mapN(_ ++ _)
                          .map(_.headOption)
      configLocation  <- serviceConfigsConnector.getConfigLocation(serviceName)
      isDecommission  <- oRepo.fold(Future.successful(false))(repo => lifecycleStatus(repo).map(Seq(LifecycleStatus.DecommissionInProgress, LifecycleStatus.Archived,LifecycleStatus.Deleted).contains))
      isFrontend      =  oRepo.flatMap(_.serviceType).contains(ServiceType.Frontend)
      isAdminFrontend =  oRepo.map(_.tags).exists(_.contains(TeamsAndRepositoriesConnector.Tag.AdminFrontend))
      githubRepo      =  checkRepoExists(oRepo)
      oMdptFrontend   <- serviceConfigsConnector
                          .getMDTPFrontendRoutes(serviceName)
                          .map(routes => Environment.values.map(env => env -> checkFrontendRouteForEnv(routes, env)).toMap)
                          .map(xs => Option.when(xs.values.exists(_.isRight) || (isFrontend && !isAdminFrontend))(xs))
      oAdminFrontend  <- serviceConfigsConnector
                          .getAdminFrontendRoutes(serviceName)
                          .map(routes => Environment.values.map(env => env -> checkAdminFrontendRouteForEnv(routes, env)).toMap)
                          .map(xs => Option.when(xs.values.exists(_.isRight) || isAdminFrontend)(xs))
      oAuthConfig     <- serviceConfigsConnector
                          .getInternalAuthConfig(serviceName)
                          .map{ configs =>
                            Map[Environment, Check.Result](
                              Environment.QA         -> checkForInternalAuthEnvironment(configs,"qa"   , Environment.QA),
                              Environment.Production -> checkForInternalAuthEnvironment(configs, "prod", Environment.Production))}
                          .map(xs => Option.when(xs.values.exists(_.isRight))(xs))
      pipelineJob     <- checkPipelineJob(serviceName)
      deploymentEnv   <- releasesConnector
                          .getReleases(serviceName)
                          .map(releases => Environment.values.map(env => env -> checkIsDeployedForEnv(serviceName, releases.versions, env)).toMap)
      mongoDb         <- serviceMetricsConnector
                           .getCollections(serviceName)
                           .map(mcss => Environment.values.map(env => env -> checkMongoDbExistsInEnv(mcss, env)).toMap)
      allChecks        = SimpleCheck(
                           title      = "Github Repo",
                           result     = githubRepo,
                           helpText   = "Has the Github repository for the service been created? This is where the source code for the service is stored and managed.",
                           linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/raise-a-decommissioning-request.html#content")
                                        else                Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-github-repository.html")
                         ) ::
                         SimpleCheck(
                           title      = "App Config Base",
                           result     = configLocation.get("app-config-base") match {
                                          case None    => Left(Check.Missing(s"/create-app-configs?serviceName=${serviceName.asString}"))
                                          case Some(e) => Right(Check.Present(e))
                                        },
                           helpText   = "Additional configuration included in the build and applied to all environments.",
                           linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/remove-service-configuration.html")
                                        else                Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-app-config.html")
                         ) ::
                         EnvCheck(
                           title      = "App Config Environment",
                           results    = Environment.values.map(env =>
                                          env -> (
                                            configLocation.get(s"app-config-${env.asString}") match {
                                              case None    => Left(Check.Missing(s"/create-app-configs?serviceName=${serviceName.asString}"))
                                              case Some(e) => Right(Check.Present(e))
                                            }
                                          )
                                        ).toMap,
                           helpText   = "Environment specific configuration.",
                           linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/remove-service-configuration.html")
                                        else                Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-app-config.html")
                         ) ::
                         oAuthConfig.map { results =>
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
                             linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/remove-public-access-configuration.html")
                                          else                Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/allow-user-access-to-a-frontend-microservice.html")
                           )
                         }.toList :::
                         oAdminFrontend.map { results =>
                           EnvCheck(
                             title      = "Admin Frontend Routes",
                             results    = results,
                             helpText   = "Configuration required to expose the service under the admin.tax.service.gov.uk domain for internal admin users.",
                             linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/remove-public-access-configuration.html")
                                          else                Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/allow-user-access-to-a-frontend-microservice.html")
                           )
                         }.toList :::
                         SimpleCheck(
                           title      = "Build Jobs",
                           result     = configLocation.get("build-jobs") match {
                                          case None    => Left(Check.Missing("https://github.com/hmrc/build-jobs"))
                                          case Some(e) => Right(Check.Present(e))
                                        },
                           helpText   = "Configuration required to trigger test runs or deploy to pre-production environments automatically on merge.",
                           linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/remove-ci-cd-jobs-and-pipeline.html")
                                        else                Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-a-ci-cd-pipeline.html")
                         ) ::
                         SimpleCheck(
                           title      = "Pipeline Jobs",
                           result     = pipelineJob,
                           helpText   = "Configuration to automatically deploy to lower environments when a build completes",
                           linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/remove-ci-cd-jobs-and-pipeline.html")
                                        else                Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-a-ci-cd-pipeline.html#add-the-pipelinejobbuilder")
                         ) ::
                         SimpleCheck(
                           title      = "Service Manager Config",
                           result     = configLocation.get("service-manager-config") match {
                                          case None    => Left(Check.Missing("https://github.com/hmrc/service-manager-config"))
                                          case Some(e) => Right(Check.Present(e))
                                        },
                           helpText   = "Allows the service to be run with service-manager.",
                           linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/remove-microservice-from-service-manager.html")
                                        else                Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/add-a-microservice-to-service-manager.html")
                         ) ::
                         SimpleCheck(
                           title      = "Logging - Kibana",
                           result     = configLocation.get("kibana") match {
                                          case None    => Left(Check.Missing("https://github.com/hmrc/kibana-dashboards"))
                                          case Some(e) => Right(Check.Present(e))
                                        },
                           helpText   = "Creates a convenient dashboard in Kibana for the service.",
                           linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/remove-a-kibana-logs-dashboard.html")
                                        else                Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/add-a-logs-dashboard-to-Kibana.html")
                         ) ::
                         SimpleCheck(
                           title      = "Metrics - Grafana",
                           result     = configLocation.get("grafana") match {
                                          case None    => Left(Check.Missing("https://github.com/hmrc/grafana-dashboards"))
                                          case Some(e) => Right(Check.Present(e))
                                        },
                           helpText   = "Creates a dashboard in Grafana for viewing the service's metrics.",
                           linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/remove-a-grafana-metrics-dashboard.html")
                                        else                Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/add-a-metrics-dashboard-to-Grafana.html")
                         ) ::
                         SimpleCheck(
                           title      = "Alerts - PagerDuty",
                           result     = configLocation.get("alerts") match {
                                          case None    => Left(Check.Missing("https://github.com/hmrc/alert-config"))
                                          case Some(e) => Right(Check.Present(e))
                                        },
                           helpText   = "Enables and configures PagerDuty alerts for the service.",
                           linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/add-service-alerting-using-pagerduty.html")
                                        else                Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/remove-pagerduty-alerting-configuration.html")
                         ) ::
                         EnvCheck(
                           title      = "Deployed",
                           results    = deploymentEnv,
                           helpText   = "Which environments the service has been deployed to.",
                           linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/remove-microservice-from-non-prod-environments.html#get-the-latest-versions-of-all-non-production-app-config-repositories")
                                        else                Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/carry-out-an-early-deployment-to-production.html")
                         ) ::
                         EnvCheck(
                           title      = "Mongo Database",
                           results    = mongoDb,
                           helpText   = "Which environments have stored data in Mongo.",
                           linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/drop-a-non-production-mongodb-collection.html")
                                        else                None
                         ) ::
                         EnvCheck(
                           title      = "Custom Shutter Pages",
                           results    = Environment.values.map(env =>
                                          env -> (
                                            configLocation.get(s"outage-page-${env.asString}") match {
                                              case None    => Left(Check.Missing(s"https://github.com/hmrc/outage-pages/blob/main/${env.asString}"))
                                              case Some(e) => Right(Check.Present(e))
                                            }
                                          )
                                        ).toMap,
                           helpText   = "Which environments have custom shutter pages. These are optional.",
                           linkToDocs = Some("https://confluence.tools.tax.service.gov.uk/display/DTRG/Shuttering+your+service#Shutteringyourservice-Configuringshutteringformyservice")
                         ) :: {
                            val environmentChecks = Environment.values.map(env =>
                              env -> (
                                configLocation.get(s"upscan-config-${env.asString}") match {
                                  case None    => Left(Check.Missing(s"https://github.com/hmrc/upscan-app-config/blob/main/${env.asString}/verify.yaml"))
                                  case Some(e) => Right(Check.Present(e))
                                }
                              )
                            ).toMap

                            Option.when(environmentChecks.values.exists(_.isRight))(environmentChecks).map(results =>
                              EnvCheck(
                                title      = "Upscan Config",
                                results    = results,
                                helpText   = "Which environments are configured in Upscan config. These are optional.",
                                linkToDocs = if (isDecommission) Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/decommission-a-microservice/remove-upscan-configuration.html")
                                             else                Some("https://confluence.tools.tax.service.gov.uk/display/PLATOPS/How+to+allowlist+the+service+to+use+Upscan")
                              )
                            ).toList
                         } ::: Nil
      checks           = StatusCheckService.hideUnconfiguredEnvironments(allChecks, environmentsToHideWhenUnconfigured)
    } yield checks

  private def lifecycleStatus(repo: TeamsAndRepositoriesConnector.Repo)(implicit ec: ExecutionContext): Future[Lifecycle] = {
    val serviceName = ServiceName(repo.name)
    lifecycleStatusRepository
      .lastLifecycleStatus(serviceName)
      .map {
        case _    if repo.isArchived   => Lifecycle(serviceName, LifecycleStatus.Archived)
        case _    if repo.isDeleted    => Lifecycle(serviceName, LifecycleStatus.Deleted)
        case None if repo.isDeprecated => Lifecycle(serviceName, LifecycleStatus.Deprecated)
        case None                      => Lifecycle(serviceName, LifecycleStatus.Active)
        case Some(lifecycle)           => lifecycle
      }
  }

  def getLifecycleStatus(serviceName: ServiceName)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[Lifecycle]] =
    for {
      oRepo           <- ( teamsAndReposConnector.findServiceRepos(serviceName = Some(serviceName))
                         , teamsAndReposConnector.findDeletedServiceRepos(serviceName = Some(serviceName))
                         ).mapN(_ ++ _)
                          .map(_.headOption)
      lifecycleStatus <- oRepo.fold(Future.successful(Option.empty[Lifecycle]))(repo => lifecycleStatus(repo).map(Option.apply))
    } yield lifecycleStatus

  def setLifecycleStatus(
    serviceName    : ServiceName,
    lifecycleStatus: LifecycleStatus,
    username       : String
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    for {
      current <- lifecycleStatusRepository.lastLifecycleStatus(serviceName)
      _       <- lifecycleStatusRepository.setLifecycleStatus(serviceName, lifecycleStatus, username)
      _       <- if (lifecycleStatus == LifecycleStatus.DecommissionInProgress && current.forall(_.lifecycleStatus != LifecycleStatus.DecommissionInProgress)) {
                   val msg = SlackNotificationRequest.markedForDecommissioning(serviceName.asString, username)
                   slackNotificationsConnector.send(msg)
                 } else Future.unit
    } yield ()

  private def checkRepoExists(oRepo: Option[TeamsAndRepositoriesConnector.Repo]): Check.Result =
    oRepo match {
      case Some(repo) if !repo.isArchived
                      && !repo.isDeleted  => Right(Check.Present(repo.githubUrl))
      case _                              => Left(Check.Missing("/create-service"))
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

  private def checkForInternalAuthEnvironment(configs: Seq[ServiceConfigsConnector.InternalAuthConfig], internalAuthEnv: String, environment: Environment): Check.Result = {
    val url = s"https://github.com/hmrc/internal-auth-config/tree/main/$internalAuthEnv"
    if (configs.exists(cfg => cfg.environment == environment))
      Right(Check.Present(url))
    else
      Left(Check.Missing(url))
  }

  private def checkPipelineJob(serviceName: ServiceName)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Check.Result] =
    for {
      jobs <- teamsAndReposConnector.findJenkinsJobs(serviceName.asString)
      check = jobs.find(_.jobType == TeamsAndRepositoriesConnector.JobType.Pipeline)
    } yield check match {
      case Some(job) => Right(Check.Present(job.jenkinsUrl))
      case None      => Left(Check.Missing(s"https://github.com/hmrc/build-jobs"))
    }

  private def checkIsDeployedForEnv(serviceName: ServiceName, releases: Seq[ReleasesConnector.Release], env: Environment): Check.Result =
    if (releases.map(_.environment).contains(env.asString))
      Right(Check.Present(s"https://catalogue.tax.service.gov.uk/deployment-timeline?service=${serviceName.asString}"))
    else
      Left(Check.Missing(s"/deploy-service/1?serviceName=${serviceName.asString}"))

  private def checkMongoDbExistsInEnv(collections: Seq[ServiceMetricsConnector.MongoCollectionSize], env: Environment): Check.Result =
    if (collections.map(_.environment).contains(env)) {
      val db = collections.headOption.fold("")(_.database)
      Right(Check.Present(s"https://grafana.tools.${env.asString}.tax.service.gov.uk/d/platops-mongo-collections?var-replica_set=*&var-database=$db&var-collection=All&orgId=1"))
    } else Left(Check.Missing(""))
}

object StatusCheckService {

  def hideUnconfiguredEnvironments(checks: List[Check], environments: Set[Environment]): List[Check] = {
    val configured =
      checks
        .flatMap {
          case _: Check.SimpleCheck => Nil
          case x: Check.EnvCheck    => x.results.toList
        }
        .collect { case (k, v) if environments.contains(k) && v.isRight => k }
        .toSet

    checks.collect {
      case x: Check.SimpleCheck => x
      case x: Check.EnvCheck    => x.copy(results = x.results.removedAll(environments.removedAll(configured)))
    }
  }
}
