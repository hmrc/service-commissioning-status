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

case class FrontendRoutes(
  integration : Boolean,
  development : Boolean,
  qa          : Boolean,
  staging     : Boolean,
  externalTest: Boolean,
  production  : Boolean
)

object FrontendRoutes {
  implicit val frFormat: Format[FrontendRoutes] =
    Json.format[FrontendRoutes]
}

case class AppConfig(
  integration : Boolean,
  development : Boolean,
  qa          : Boolean,
  staging     : Boolean,
  externalTest: Boolean,
  production  : Boolean
)

object AppConfig {
  implicit val acFormat: Format[AppConfig] =
    Json.format[AppConfig]
}

case class DeploymentEnvironment(
  integration : Boolean,
  development : Boolean,
  qa          : Boolean,
  staging     : Boolean,
  externalTest: Boolean,
  production  : Boolean
)

object DeploymentEnvironment {
  implicit val deFormat: Format[DeploymentEnvironment] =
    Json.format[DeploymentEnvironment]
}

case class Dashboards(
  kibana       : Boolean,
  grafana      : Boolean
)

object Dashboards {
  implicit val dFormat: Format[Dashboards] =
    Json.format[Dashboards]
}

case class ServiceCommissioningStatus(
  hasRepo           : Boolean,
  hasSMConfig       : Boolean,
  hasFrontendRoutes : FrontendRoutes,
  hasAppConfig      : AppConfig,
  deployedIn        : DeploymentEnvironment,
  hasDashboards     : Dashboards,
  hasBuildJobs      : Boolean,
  hasAlerts         : Boolean
)

object ServiceCommissioningStatus {

  implicit val apiFormat: Format[ServiceCommissioningStatus] =
    Json.format[ServiceCommissioningStatus]

//  val apiFormat: Format[ServiceCommissioningStatus] =
//    ( (__ \ "hasRepo"     ).format[Boolean]
//      ~ (__ \ "inSMConfig").format[Boolean]
//      ~ (__ \ "hasMDTPFrontendRoutes").formatNullable[Option[Boolean]]
//      )(ServiceCommissioningStatus.apply, unlift(ServiceCommissioningStatus.unapply))

}