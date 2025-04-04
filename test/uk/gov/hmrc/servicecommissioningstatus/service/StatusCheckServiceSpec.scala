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

import org.mockito.Mockito.*
import org.scalatestplus.mockito.MockitoSugar
import org.mockito.ArgumentMatchers.any
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.servicecommissioningstatus._
import uk.gov.hmrc.servicecommissioningstatus.connectors._
import uk.gov.hmrc.servicecommissioningstatus.persistence._
import uk.gov.hmrc.servicecommissioningstatus.persistence.LifecycleStatusRepository.Lifecycle

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

class StatusCheckServiceSpec extends AnyWordSpec with Matchers with ScalaFutures:
  import Check._
  import Environment._

  private val missing: Result = Result.Missing("")
  private val present: Result = Result.Present("")

  private val allPresent: Map[Environment, Result] =
    Map(Integration -> present, Development -> present, QA -> present, Staging -> present, ExternalTest -> present, Production -> present)

  private val allMissing: Map[Environment, Result] =
    Map(Integration -> missing, Development -> missing, QA -> missing, Staging -> missing, ExternalTest -> missing, Production -> missing)

  "Checks" should:
    "hide Integration when unconfigured" in:
      StatusCheckService.hideUnconfiguredEnvironments(
        checks       =  SimpleCheck(
                          title      = "A",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "B",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "C",
                          results    = allPresent + (Integration -> missing),
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "D",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "E",
                          results    = allPresent + (Integration -> missing),
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "F",
                          results    = allPresent + (Integration -> missing),
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "G",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        Nil
      , environments = Set(Environment.Integration, Environment.Development)
      ) shouldBe (
        SimpleCheck(
          title      = "A",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "B",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "C",
          results    = allPresent - Integration,
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "D",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "E",
          results    = allPresent - Integration,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "F",
          results    = allPresent - Integration,
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "G",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        Nil
      )

    "show Integration when partially configured" in:
      val checks: List[Check] =
        SimpleCheck(
          title      = "A",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "B",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "C",
          results    = allPresent + (Integration -> missing),
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "D",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "E",
          results    = allPresent, // Configured here
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "F",
          results    = allPresent + (Integration -> missing),
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "G",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        Nil
      StatusCheckService.hideUnconfiguredEnvironments(
        checks       = checks
      , environments = Set(Environment.Integration, Environment.Development)
      ) shouldBe (checks) // Unchanged

    "hide Integration and Development unconfigured" in:
      StatusCheckService.hideUnconfiguredEnvironments(
        checks       =  SimpleCheck(
                          title      = "A",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "B",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "C",
                          results    = allPresent ++ Map(Integration -> missing, Development -> missing),
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "D",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "E",
                          results    = allPresent ++ Map(Integration -> missing, Development -> missing),
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "F",
                          results    = allPresent ++ Map(Integration -> missing, Development -> missing),
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "G",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        Nil
      , environments = Set(Environment.Integration, Environment.Development)
      ) shouldBe (
        SimpleCheck(
          title      = "A",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "B",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "C",
          results    = allPresent.removedAll(List(Integration, Development)),
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "D",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "E",
          results    = allPresent.removedAll(List(Integration, Development)),
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "F",
          results    = allPresent.removedAll(List(Integration, Development)),
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "G",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        Nil
      )

    "show QA, Staging, ExternalTest & Production when unconfigured" in:
      StatusCheckService.hideUnconfiguredEnvironments(
        checks       =  SimpleCheck(
                          title      = "A",
                          result     = missing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "B",
                          result     = missing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "C",
                          results    = allMissing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "D",
                          result     = missing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "E",
                          results    = allMissing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "F",
                          results    = allMissing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "G",
                          result     = missing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        Nil
      , environments = Set(Environment.Integration, Environment.Development)
      ) shouldBe (
        SimpleCheck(
          title      = "A",
          result     = missing,
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "B",
          result     = missing,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "C",
          results    = allMissing.removedAll(List(Integration, Development)),
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "D",
          result     = missing,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "E",
          results    = allMissing.removedAll(List(Integration, Development)),
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "F",
          results    = allMissing.removedAll(List(Integration, Development)),
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "G",
          result     = missing,
          helpText   = "",
          linkToDocs = None
        ) ::
        Nil
      )

  "lifecycleStatus" should:

    "return the status Archived" when:
      "the service is archived" in new StatusCheckServiceFixture(isArchived = true):
        val status: Option[Lifecycle] =
          service.getLifecycleStatus(serviceName).futureValue

        status shouldBe Some(Lifecycle(ServiceName("serviceName"), LifecycleStatus.Archived))

      "the service is archived and marked for decommission" in new StatusCheckServiceFixture(
        isArchived              = true,
        isMarkedForDecommission = true
      ):
        val status: Option[Lifecycle] =
          service.getLifecycleStatus(serviceName).futureValue

        status shouldBe Some(Lifecycle(ServiceName("serviceName"), LifecycleStatus.Archived))

      "the service is archived, marked for decommission and deprecated" in new StatusCheckServiceFixture(
          isArchived              = true,
          isMarkedForDecommission = true,
          isDeprecated            = true,
        ):
        val status: Option[Lifecycle] =
          service.getLifecycleStatus(serviceName).futureValue

        status shouldBe Some(Lifecycle(ServiceName("serviceName"), LifecycleStatus.Archived))

    "return the status DecommissionInProgress" when:

      val now: Instant = Instant.now()

      "the service is marked for decommission" in new StatusCheckServiceFixture(isMarkedForDecommission = true, dateTime = now):
        val status: Option[Lifecycle] =
          service.getLifecycleStatus(serviceName).futureValue

        status shouldBe Some(Lifecycle(ServiceName("serviceName"), LifecycleStatus.DecommissionInProgress, Some("bar"), Some(now)))

      "the service is deprecated and marked for decommission" in new StatusCheckServiceFixture(
        isMarkedForDecommission = true,
        isDeprecated            = true,
        dateTime                = now
      ):
        val status: Option[Lifecycle] =
          service.getLifecycleStatus(serviceName).futureValue

        status shouldBe Some(Lifecycle(ServiceName("serviceName"), LifecycleStatus.DecommissionInProgress , Some("bar"), Some(now)))

    "return the status Deprecated" when:
      "the service is deprecated and not archived or marked for decommission" in new StatusCheckServiceFixture(isDeprecated = true):
        val status: Option[Lifecycle] =
          service.getLifecycleStatus(serviceName).futureValue

        status shouldBe Some(Lifecycle(ServiceName("serviceName"), LifecycleStatus.Deprecated))

    "return Active" when:
      "the service is not archived, deprecated or marked for decommission" in new StatusCheckServiceFixture:
        val status: Option[Lifecycle] =
          service.getLifecycleStatus(serviceName).futureValue

        status shouldBe Some(Lifecycle(ServiceName("serviceName"), LifecycleStatus.Active))

    "send a slack message" when:
      "the service is marked for decommissioning" in new StatusCheckServiceFixture():
        service.setLifecycleStatus(serviceName, LifecycleStatus.DecommissionInProgress, "timmy.test").futureValue

        verify(slackNotificationsConnector, times(1)).send(any[SlackNotificationRequest])(using any[HeaderCarrier])

    "not send a slack message" when:
      "the status isn't decommissioning" in new StatusCheckServiceFixture():
        service.setLifecycleStatus(serviceName, LifecycleStatus.Archived, "timmy.test").futureValue

        verify(slackNotificationsConnector, times(0)).send(any[SlackNotificationRequest])(using any[HeaderCarrier])

      "the service has already been marked for decommissioning" in new StatusCheckServiceFixture(isMarkedForDecommission = true):
        service.setLifecycleStatus(serviceName, LifecycleStatus.DecommissionInProgress, "timmy.test").futureValue

        verify(slackNotificationsConnector, times(0)).send(any[SlackNotificationRequest])(using any[HeaderCarrier])

  private abstract class StatusCheckServiceFixture(
    isArchived             : Boolean = false,
    isDeprecated           : Boolean = false,
    isMarkedForDecommission: Boolean = false,
    isDeleted              : Boolean = false,
    dateTime               : Instant = Instant.now()
  ) extends MockitoSugar:
    private val config                       : Configuration                 = mock[Configuration]
    private val serviceConfigsConnector      : ServiceConfigsConnector       = mock[ServiceConfigsConnector]
    private val releasesConnector            : ReleasesConnector             = mock[ReleasesConnector]
    private val teamsAndReposConnector       : TeamsAndRepositoriesConnector = mock[TeamsAndRepositoriesConnector]
    private val serviceMetricsConnector      : ServiceMetricsConnector       = mock[ServiceMetricsConnector]
    private val cachedRepository             : CacheRepository               = mock[CacheRepository]
    private val lifecycleStatusRepository    : LifecycleStatusRepository     = mock[LifecycleStatusRepository]

    protected val serviceName                : ServiceName                   = ServiceName("serviceName")
    protected val slackNotificationsConnector: SlackNotificationsConnector   = mock[SlackNotificationsConnector]

    protected val service: StatusCheckService =
      StatusCheckService(
        config,
        serviceConfigsConnector,
        releasesConnector,
        teamsAndReposConnector,
        serviceMetricsConnector,
        slackNotificationsConnector,
        cachedRepository,
        lifecycleStatusRepository,
      )

    given HeaderCarrier = HeaderCarrier()
    given ExecutionContext = scala.concurrent.ExecutionContext.global

    when(teamsAndReposConnector.findServiceRepos(any[Option[ServiceName]], any[Option[TeamName]], any[Option[DigitalService]], any[Option[ServiceType]])(using any[HeaderCarrier]))
      .thenReturn(Future.successful(Seq(TeamsAndRepositoriesConnector.Repo(
        name         = serviceName.asString,
        githubUrl    = "github.url",
        isArchived   = isArchived,
        isDeprecated = isDeprecated,
        isDeleted    = isDeleted,
      ))))

    when(teamsAndReposConnector.findDeletedServiceRepos(any[Option[ServiceName]], any[Option[TeamName]], any[Option[DigitalService]], any[Option[ServiceType]])(using any[HeaderCarrier]))
      .thenReturn(Future.successful(Nil))

    when(lifecycleStatusRepository.lastLifecycleStatus(any[ServiceName]))
      .thenReturn(Future.successful(Option.when(isMarkedForDecommission)(LifecycleStatusRepository.Lifecycle(serviceName, LifecycleStatus.DecommissionInProgress, Some("bar"), Some(dateTime)))))

    when(lifecycleStatusRepository.setLifecycleStatus(any[ServiceName], any[LifecycleStatus], any[String]))
      .thenReturn(Future.unit)

    when(slackNotificationsConnector.send(any[SlackNotificationRequest])(using any[HeaderCarrier]))
      .thenReturn(Future.successful(SlackNotificationResponse(errors = List.empty)))
