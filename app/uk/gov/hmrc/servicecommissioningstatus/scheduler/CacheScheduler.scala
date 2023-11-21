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

package uk.gov.hmrc.servicecommissioningstatus.scheduler

import akka.actor.ActorSystem
import play.api.Configuration
import play.api.inject.ApplicationLifecycle
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.TimestampSupport
import uk.gov.hmrc.mongo.lock.{MongoLockRepository, ScheduledLockService}
import uk.gov.hmrc.servicecommissioningstatus.service.StatusCheckService

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{DurationInt, FiniteDuration}

@Singleton
class CacheScheduler @Inject()(
  configuration       : Configuration
, mongoLockRepository : MongoLockRepository
, statusCheckService  : StatusCheckService
, timestampSupport    : TimestampSupport
)(implicit
  ec                  : ExecutionContext
, actorSystem         : ActorSystem
, applicationLifecycle: ApplicationLifecycle
) extends SchedulerUtils {

  private val schedulerConfig = SchedulerConfig(
    enabledKey   = "cacheScheduler.enabled"
  , enabled      = configuration.get[Boolean       ]("cacheScheduler.enabled")
  , interval     = configuration.get[FiniteDuration]("cacheScheduler.interval")
  , initialDelay = configuration.get[FiniteDuration]("cacheScheduler.initialDelay")
  )

  private val lock = ScheduledLockService(
    lockRepository    = mongoLockRepository
  , lockId            = "slack-message-scheduler"
  , timestampSupport  = timestampSupport
  , schedulerInterval = schedulerConfig.interval
  )

  implicit val hc: HeaderCarrier = HeaderCarrier()

  scheduleWithLock("Cache Scheduler", schedulerConfig, lock) {
    logger.info("Updating cache ...")
    for {
      count <- statusCheckService.updateCache()
      _ =  logger.info(s"Finished updating cache - updated ${count} services")
    } yield ()
  }

}
