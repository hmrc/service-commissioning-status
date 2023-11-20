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

import play.api.Configuration
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.servicecommissioningstatus.connectors._
import uk.gov.hmrc.servicecommissioningstatus.{Check, Environment, TeamName, ServiceType, ServiceName}
import uk.gov.hmrc.servicecommissioningstatus.persistence.CacheRepository

import cats.implicits._

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StatusCheckService @Inject()(
  config                 : Configuration
, serviceConfigsConnector: ServiceConfigsConnector
, releasesConnector      : ReleasesConnector
, teamsAndReposConnector : TeamsAndRepositoriesConnector
, serviceMetricsConnector: ServiceMetricsConnector
, cachedRepository       : CacheRepository
)(implicit ec: ExecutionContext){

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
    Nil

  def updateCache()(implicit hc: HeaderCarrier): Future[Int] =
    for {
      services <- teamsAndReposConnector.findServiceRepos()
      results  <- services.foldLeftM[Future, Seq[CacheRepository.ServiceCheck]](List.empty) { (acc, repo) =>
                    commissioningStatusChecks(ServiceName(repo.name))
                      .map(checks => { acc :+ CacheRepository.ServiceCheck(ServiceName(repo.name), checks) })
                  }
      _        <- cachedRepository.putAll(results)
    } yield results.size

  def cachedCommissioningStatusChecks(teamName: Option[TeamName], serviceType: Option[ServiceType])(implicit hc: HeaderCarrier): Future[Seq[CacheRepository.ServiceCheck]] =
    for {
      services  <- teamsAndReposConnector.findServiceRepos(
                     team        = teamName
                   , serviceType = serviceType
                   )
      results   <- cachedRepository.findAll(services.map(repo => ServiceName(repo.name)))
    } yield results

  private val environmentsToHideWhenUnconfigured: Set[Environment] = {
    import scala.jdk.CollectionConverters._
    config.underlying.getStringList("environmentsToHideWhenUnconfigured").asScala.toSet.map { str: String =>
      Environment.parse(str).getOrElse(sys.error(s"config 'environmentsToHideWhenUnconfigured' contains an invalid environment: $str"))
    }
  }

  import Check.{EnvCheck, SimpleCheck}
  def commissioningStatusChecks(serviceName: ServiceName)(implicit hc: HeaderCarrier): Future[List[Check]] =
    for {
      oRepo           <- teamsAndReposConnector.findServiceRepos(name = Some(serviceName.asString)).map(_.headOption)
      configLocation  <- serviceConfigsConnector.getConfigLocation(serviceName)
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
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-github-repository.html")
                         ) ::
                         SimpleCheck(
                           title      = "App Config Base",
                           result     = configLocation.get("app-config-base") match {
                                          case None if Switches.catalogueCreateAppConfig.isEnabled => Left(Check.Missing(s"/create-app-configs?serviceName=${serviceName.asString}"))
                                          case None    => Left(Check.Missing("https://build.tax.service.gov.uk/job/PlatOps/job/Tools/job/create-app-configs/build"))
                                          case Some(e) => Right(Check.Present(e))
                                        },
                           helpText   = "Additional configuration included in the build and applied to all environments.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-app-config.html")
                         ) ::
                         EnvCheck(
                           title      = "App Config Environment",
                           results    = Environment.values.map(env =>
                                          env -> (
                                            configLocation.get(s"app-config-${env.asString}") match {
                                              case None if Switches.catalogueCreateAppConfig.isEnabled => Left(Check.Missing(s"/create-app-configs?serviceName=${serviceName.asString}"))
                                              case None    => Left(Check.Missing("https://build.tax.service.gov.uk/job/PlatOps/job/Tools/job/create-app-configs/build"))
                                              case Some(e) => Right(Check.Present(e))
                                            }
                                          )
                                        ).toMap,
                           helpText   = "Environment specific configuration.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/create-app-config.html")
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
                             linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/allow-user-access-to-a-frontend-microservice.html")
                           )
                         }.toList :::
                         oAdminFrontend.map { results =>
                           EnvCheck(
                             title      = "Admin Frontend Routes",
                             results    = results,
                             helpText   = "Configuration required to expose the service under the admin.tax.service.gov.uk domain for internal admin users.",
                             linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/allow-user-access-to-a-frontend-microservice.html")
                           )
                         }.toList :::
                         SimpleCheck(
                           title      = "Build Jobs",
                           result     = configLocation.get("build-jobs") match {
                                          case None    => Left(Check.Missing("https://github.com/hmrc/build-jobs"))
                                          case Some(e) => Right(Check.Present(e))
                                        },
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
                           title      = "Service Manager Config",
                           result     = configLocation.get("https://github.com/hmrc/service-manager-config") match {
                                          case None    => Left(Check.Missing("https://github.com/hmrc/service-manager-config"))
                                          case Some(e) => Right(Check.Present(e))
                                        },
                           helpText   = "Allows the service to be run with service-manager.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/add-a-microservice-to-service-manager.html")
                         ) ::
                         SimpleCheck(
                           title      = "Logging - Kibana",
                           result     = configLocation.get("kibana") match {
                                          case None    => Left(Check.Missing("https://github.com/hmrc/kibana-dashboards"))
                                          case Some(e) => Right(Check.Present(e))
                                        },
                           helpText   = "Creates a convenient dashboard in Kibana for the service.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/add-a-logs-dashboard-to-Kibana.html")
                         ) ::
                         SimpleCheck(
                           title      = "Metrics - Grafana",
                           result     = configLocation.get("grafana") match {
                                          case None    => Left(Check.Missing("https://github.com/hmrc/grafana-dashboards"))
                                          case Some(e) => Right(Check.Present(e))
                                        },
                           helpText   = "Creates a dashboard in Grafana for viewing the service's metrics.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/add-a-metrics-dashboard-to-Grafana.html")
                         ) ::
                         SimpleCheck(
                           title      = "Alerts - PagerDuty",
                           result     = configLocation.get("alerts") match {
                                          case None    => Left(Check.Missing("https://github.com/hmrc/alert-config"))
                                          case Some(e) => Right(Check.Present(e))
                                        },
                           helpText   = "Enables and configures PagerDuty alerts for the service.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/add-service-alerting-using-pagerduty.html")
                         ) ::
                         EnvCheck(
                           title      = "Deployed",
                           results    = deploymentEnv,
                           helpText   = "Which environments the service has been deployed to.",
                           linkToDocs = Some("https://docs.tax.service.gov.uk/mdtp-handbook/documentation/create-a-microservice/carry-out-an-early-deployment-to-production.html")
                         ) ::
                         EnvCheck(
                           title      = "Mongo Database",
                           results    = mongoDb,
                           helpText   = "Which environments have stored data in Mongo.",
                           linkToDocs = None
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
                         ) ::
                         Nil
      checks           = StatusCheckService.hideUnconfiguredEnvironments(allChecks, environmentsToHideWhenUnconfigured)
    } yield checks

  private def checkRepoExists(oRepo: Option[TeamsAndRepositoriesConnector.Repo]): Check.Result =
    oRepo match {
      case Some(repo) if !repo.isArchived => Right(Check.Present(repo.githubUrl))
      case _ if Switches.catalogueCreateRepo.isEnabled => Left(Check.Missing("/create-service"))
      case _                                           => Left(Check.Missing("https://build.tax.service.gov.uk/job/PlatOps/job/Tools/job/create-a-repository/build"))
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

  private def checkPipelineJob(serviceName: ServiceName)(implicit hc: HeaderCarrier): Future[Check.Result] =
    for {
      jobs <- teamsAndReposConnector.findBuildJobs(serviceName.asString)
      check = jobs.find(_.jobType == TeamsAndRepositoriesConnector.BuildJobType.Pipeline)
    } yield check match {
      case Some(job) => Right(Check.Present(job.jenkinsUrl))
      case None      => Left(Check.Missing(s"https://github.com/hmrc/build-jobs"))
    }

  private def checkIsDeployedForEnv(serviceName: ServiceName, releases: Seq[ReleasesConnector.Release], env: Environment): Check.Result =
    if (releases.map(_.environment).contains(env.asString))
      Right(Check.Present(s"https://catalogue.tax.service.gov.uk/deployment-timeline?service=${serviceName.asString}"))
    else if (Switches.catalogueDeployService.isEnabled)
      Left(Check.Missing(s"/deploy-service/1?serviceName=${serviceName.asString}"))
    else
      Left(Check.Missing(s"https://build.tax.service.gov.uk/job/build-and-deploy/job/deploy-microservice/build"))

  private def checkMongoDbExistsInEnv(collections: Seq[ServiceMetricsConnector.MongoCollectionSize], env: Environment): Check.Result =
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

case class FeatureSwitch(name: String, isEnabled: Boolean)

object FeatureSwitch {

  def forName(name: String) = FeatureSwitch(name, java.lang.Boolean.getBoolean(systemPropertyName(name)))

  def enable(switch: FeatureSwitch): FeatureSwitch = setProp(switch.name, true)

  def disable(switch: FeatureSwitch): FeatureSwitch = setProp(switch.name, false)

  private def setProp(name: String, value: Boolean): FeatureSwitch = {
    sys.props += ((systemPropertyName(name), value.toString))
    forName(name)
  }

  private def systemPropertyName(name: String) = s"feature.$name"

}

object Switches {

  def catalogueCreateRepo: FeatureSwitch =
    FeatureSwitch.forName("catalogue-create-repo")

  def catalogueDeployService: FeatureSwitch =
    FeatureSwitch.forName("catalogue-deploy-service")

  def catalogueCreateAppConfig: FeatureSwitch =
    FeatureSwitch.forName("catalogue-create-app-config")
}
