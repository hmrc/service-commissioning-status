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
import uk.gov.hmrc.servicecommissioningstatus.connectors.{GitHubConnector, ReleasesConnector, ServiceConfigsConnector, ServiceMetricsConnector, TeamsAndRepositoriesConnector}
import uk.gov.hmrc.servicecommissioningstatus.model.{Check, Environment}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StatusCheckService @Inject()(
  config                  : Configuration,
  gitHubConnector         : GitHubConnector,
  serviceConfigsConnector : ServiceConfigsConnector,
  releasesConnector       : ReleasesConnector,
  teamsAndReposConnector  : TeamsAndRepositoriesConnector,
  serviceMetricsConnector : ServiceMetricsConnector
)(implicit ec: ExecutionContext){

  import scala.jdk.CollectionConverters._
  private val environmentsToHideWhenUnconfigured: Set[Environment] =
    config.underlying.getStringList("environmentsToHideWhenUnconfigured").asScala.toSet.map { str: String =>
      Environment.parse(str).getOrElse(sys.error(s"config 'environmentsToHideWhenUnconfigured' contains an invalid environment: $str"))
    }

  import Check.{SimpleCheck, EnvCheck}
  def commissioningStatusChecks(serviceName: String)(implicit hc: HeaderCarrier): Future[List[Check]] =
    for {
      oRepo           <- teamsAndReposConnector.findRepo(serviceName)
      isFrontend      =  oRepo.map(_.serviceType).exists(_ == TeamsAndRepositoriesConnector.ServiceType.Frontend)
      isAdminFrontend =  oRepo.map(_.tags       ).exists(_.contains(TeamsAndRepositoriesConnector.Tag.AdminFrontend))

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
                          .map(xs => Option.when(xs.values.filter(_.isRight).nonEmpty || (isFrontend && !isAdminFrontend))(xs))
      oAdminFrontend  <- serviceConfigsConnector
                          .getAdminFrontendRoutes(serviceName)
                          .map(routes => Environment.values.map(env => env -> checkAdminFrontendRouteForEnv(routes, env)).toMap)
                          .map(xs => Option.when(xs.values.filter(_.isRight).nonEmpty || isAdminFrontend)(xs))
      buildJobs       <- serviceConfigsConnector.getBuildJobs(serviceName)
      orchestratorJob <- checkOrchestratorJob(serviceName)
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
      allChecks        = SimpleCheck(title = "Github Repo"           , result  = githubRepo     ) ::
                         SimpleCheck(title = "App Config Base"       , result  = appConfigBase  ) ::
                         EnvCheck   (title = "App Config Environment", results = appConfigEnvs  ) ::
                         oMdptFrontend.map( x => EnvCheck(title = "Frontend Routes",       results = x )).toList :::
                         oAdminFrontend.map(x => EnvCheck(title = "Admin Frontend Routes", results = x )).toList :::
                         SimpleCheck(title = "Build Jobs"            , result  = buildJobs      ) ::
                         SimpleCheck (title = "Orchestrator Jobs"    , result  = orchestratorJob) ::
                         SimpleCheck(title = "Service Manager Config", result  = smConfig       ) ::
                         SimpleCheck(title = "Logging - Kibana"      , result  = kibana         ) ::
                         SimpleCheck(title = "Metrics - Grafana"     , result  = grafana        ) ::
                         SimpleCheck(title = "Alerts - PagerDuty"    , result  = alertConfig    ) ::
                         EnvCheck   (title = "Deployed"              , results = deploymentEnv  ) ::
                         EnvCheck   (title = "Mongo Database"        , results = mongoDb        ) ::
                         Nil
      checks           = StatusCheckService.hideUnconfiguredEnvironments(allChecks, environmentsToHideWhenUnconfigured)
    } yield checks

  private def checkRepoExists(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    OptionT(gitHubConnector.getGithubRaw(s"/hmrc/$serviceName/main/repository.yaml"))
      .orElse(OptionT(gitHubConnector.getGithubApi(s"/repos/hmrc/$serviceName")))
      .value
      .map {
        case Some(_) => Right(Check.Present(s"https://github.com/hmrc/$serviceName"))
        case None    => Left(Check.Missing("https://build.tax.service.gov.uk/job/PlatOps/job/Tools/job/create-a-repository/build"))
      }

  private def checkAppConfigBaseExists(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    gitHubConnector
      .getGithubRaw(s"/hmrc/app-config-base/main/$serviceName.conf")
      .map {
        case Some(_) => Right(Check.Present(s"https://github.com/hmrc/app-config-base/blob/main/$serviceName.conf"))
        case None    => Left(Check.Missing("https://github.com/hmrc/app-config-base"))
      }

  private def checkAppConfigExistsForEnv(serviceName: String, env: Environment)(implicit hc: HeaderCarrier): Future[Check.Result] =
    gitHubConnector
      .getGithubRaw(s"/hmrc/app-config-${env.asString}/main/$serviceName.yaml")
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

  private def checkOrchestratorJob(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    for {
      optStr  <- gitHubConnector.getGithubRaw("/hmrc/orchestrator-jobs/main/src/main/groovy/uk/gov/hmrc/orchestratorjobs/Microservices.groovy")
      link     = "https://github.com/hmrc/orchestrator-jobs/blob/main/src/main/groovy/uk/gov/hmrc/orchestratorjobs/Microservices.groovy"
      evidence = optStr
                  .getOrElse("")
                  .linesIterator
                  .zipWithIndex
                  .find { case (line, _) => line.contains(s"\'$serviceName\'") }
                  .map  { case (_, idx)  => s"$link#L${idx + 1}" }
    } yield evidence match {
      case Some(e) => Right(Check.Present(e))
      case None    => Left(Check.Missing(link))
    }

  private def checkServiceManagerConfigExists(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    for {
      optStr  <- gitHubConnector.getGithubRaw("/hmrc/service-manager-config/main/services.json")
      key      = serviceName.toUpperCase.replaceAll("[-]", "_")
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
      Left(Check.Missing(s"https://orchestrator.tools.${env.asString}.tax.service.gov.uk/job/deploy-microservice"))

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
