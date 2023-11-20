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
import play.api.Logging
import play.api.inject.ApplicationLifecycle
import uk.gov.hmrc.servicecommissioningstatus.scheduler.TimePeriodLockService

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

trait SchedulerUtils extends Logging {

  private def schedule(
    label          : String,
    schedulerConfig: SchedulerConfig
  )(f: => Future[Unit]
  )(implicit actorSystem: ActorSystem, applicationLifecycle: ApplicationLifecycle, ec: ExecutionContext): Unit =
    if (schedulerConfig.enabled) {
      val initialDelay = schedulerConfig.initialDelay
      val interval     = schedulerConfig.interval
      logger.info(s"Enabling $label scheduler, running every $interval (after initial delay $initialDelay)")
      val cancellable =
        actorSystem.scheduler.scheduleWithFixedDelay(initialDelay, interval)(
          () => {
            val start = System.currentTimeMillis
            logger.info(s"Scheduler $label started")
            f.map { res =>
                logger.info(s"Scheduler $label finished - took ${System.currentTimeMillis - start} millis")
                res
              }
              .recover {
                case e => logger.error(s"$label interrupted after ${System.currentTimeMillis - start} millis because: ${e.getMessage}", e)
              }
          }
        )
      applicationLifecycle.addStopHook(() => Future(cancellable.cancel()))
    } else
      logger.info(s"$label scheduler is DISABLED. to enable, configure configure ${schedulerConfig.enabledKey}=true in config.")

  def scheduleWithTimePeriodLock(
    label          : String
  , schedulerConfig: SchedulerConfig
  , lock           : TimePeriodLockService
  )(f: => Future[Unit]
  )(implicit
    actorSystem         : ActorSystem,
    applicationLifecycle: ApplicationLifecycle,
    ec                  : ExecutionContext
  ): Unit =
    schedule(label, schedulerConfig) {
      lock.withRenewedLock(f).map {
        case Some(_) => logger.debug(s"$label finished - releasing lock")
        case None => logger.debug(s"$label cannot run - lock ${lock.lockId} is taken... skipping update")
      }
    }
}

object SchedulerUtils extends SchedulerUtils

case class SchedulerConfig(
  enabledKey  : String,
  enabled     : Boolean,
  interval    : FiniteDuration,
  initialDelay: FiniteDuration
)