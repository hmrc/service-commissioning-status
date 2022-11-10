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

package uk.gov.hmrc.servicecommissioningstatus.model

import play.api.libs.json.{Format, Json, __}

case class StatusCheck(
  status  : Boolean
, evidence: Option[String]
)

object StatusCheck {
  implicit val scFormat: Format[StatusCheck] =
    Json.format[StatusCheck]
}

case class FrontendRoutes(
  integration : StatusCheck
, development : StatusCheck
, qa          : StatusCheck
, staging     : StatusCheck
, externalTest: StatusCheck
, production  : StatusCheck
)

object FrontendRoutes {
  implicit val frFormat: Format[FrontendRoutes] =
    Json.format[FrontendRoutes]
}

case class AppConfigEnvironment(
  integration : StatusCheck
, development : StatusCheck
, qa          : StatusCheck
, staging     : StatusCheck
, externalTest: StatusCheck
, production  : StatusCheck
)

object AppConfigEnvironment {
  implicit val acFormat: Format[AppConfigEnvironment] =
    Json.format[AppConfigEnvironment]
}

case class DeploymentEnvironment(
  integration : StatusCheck
, development : StatusCheck
, qa          : StatusCheck
, staging     : StatusCheck
, externalTest: StatusCheck
, production  : StatusCheck
)

object DeploymentEnvironment {
  implicit val deFormat: Format[DeploymentEnvironment] =
    Json.format[DeploymentEnvironment]
}

case class Dashboards(
  kibana  : StatusCheck
, grafana : StatusCheck
)

object Dashboards {
  implicit val dFormat: Format[Dashboards] =
    Json.format[Dashboards]
}

case class ServiceCommissioningStatus(
  serviceName      : String
, hasRepo          : StatusCheck
, hasSMConfig      : StatusCheck
, hasFrontendRoutes: FrontendRoutes
, hasAppConfigBase : StatusCheck
, hasAppConfigEnv  : AppConfigEnvironment
, deployed         : DeploymentEnvironment
, hasDashboards    : Dashboards
, hasBuildJobs     : StatusCheck
, hasAlerts        : StatusCheck
)

object ServiceCommissioningStatus {

  implicit val apiFormat: Format[ServiceCommissioningStatus] =
    Json.format[ServiceCommissioningStatus]
}