package uk.gov.hmrc.servicecommissioningstatus.service

import uk.gov.hmrc.servicecommissioningstatus.connectors.GitHubConnector
import uk.gov.hmrc.servicecommissioningstatus.model.ServiceCommissioningStatus

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StatusCheckService @Inject()(
                                  gitHubConnector: GitHubConnector
                                  )(implicit ec: ExecutionContext){

  def commissioningChecks(serviceName: String): Future[ServiceCommissioningStatus] = {
    for {
      repoExists <- repoExists(serviceName)
    } yield ServiceCommissioningStatus()
  }


  private def repoExists(serviceName: String): Future[Boolean] = ???

  private def existsWithinSMConfig(serviceName: String): Boolean = ???

  private def hasAppConfig(serviceName: String, env: String): Future[Boolean] = ???

  private def isFrontend(serviceName: String): Boolean = ???

  private def hasFrontendRoutes(serviceName: String): Boolean = ???

  private def hasBuildJobs(serviceName: String): Boolean = ???

  private def isDeployed(serviceName: String, env: String): Future[Boolean] = ???

  private def alertConfigExists(serviceName: String): Boolean = ???

  private def hasKibanaDashboard(serviceName: String): Boolean = ???

  private def hasGrafanaDashboard(serviceName: String): Boolean = ???





}
