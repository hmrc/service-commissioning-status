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

import play.api.libs.functional.syntax.{toFunctionalBuilderOps, unlift}
import play.api.libs.json.{Writes, __}

case class StatusCheck(evidence: Option[String]) {
  lazy val exists: Boolean =
    evidence.isDefined
}

object StatusCheck {
  val writes: Writes[StatusCheck] =
    ( (__ \ "evidence"  ).writeNullable[String]
      ~ (__ \ "status"  ).write[Boolean]
      )(sc => (sc.evidence, sc.exists))
}

case class FrontendRoutes(asMap: Map[Environment, StatusCheck])

case class DeploymentEnvironment(asMap: Map[Environment, StatusCheck])

case class AppConfigEnvironment(asMap: Map[Environment, StatusCheck])

case class Dashboard(
  kibana  : StatusCheck
, grafana : StatusCheck
)

object Dashboard {
  val writes: Writes[Dashboard] = {
    implicit val scWrites: Writes[StatusCheck] = StatusCheck.writes
    ((__ \ "kibana").write[StatusCheck]
      ~ (__ \ "grafana").write[StatusCheck]
      ) (unlift(Dashboard.unapply))
  }
}

case class ServiceCommissioningStatus(
  serviceName      : String
, hasRepo          : StatusCheck
, hasSMConfig      : StatusCheck
, hasFrontendRoutes: FrontendRoutes
, hasAppConfigBase : StatusCheck
, hasAppConfigEnv  : AppConfigEnvironment
, isDeployed       : DeploymentEnvironment
, hasDashboards    : Dashboard
, hasBuildJobs     : StatusCheck
, hasAlerts        : StatusCheck
)

object ServiceCommissioningStatus {

  private implicit val scWrites: Writes[StatusCheck] = StatusCheck.writes
  private implicit val dsWrites: Writes[Dashboard] = Dashboard.writes

  private val mapFormat: Writes[Map[Environment, StatusCheck]] =
    Writes
      .of[Map[String, StatusCheck]]
      .contramap(
        _.map { case (k, v) => (k.asString, v) }
      )

  private implicit val frWrites: Writes[FrontendRoutes] =
    mapFormat.contramap(unlift(FrontendRoutes.unapply))

  private implicit val deWrites: Writes[DeploymentEnvironment] =
    mapFormat.contramap(unlift(DeploymentEnvironment.unapply))

  private implicit val acWrites: Writes[AppConfigEnvironment] =
    mapFormat.contramap(unlift(AppConfigEnvironment.unapply))


  val writes: Writes[ServiceCommissioningStatus] =
    ( (__ \ "serviceName"        ).write[String]
      ~ (__ \ "hasRepo"          ).write[StatusCheck]
      ~ (__ \ "hasSMConfig"      ).write[StatusCheck]
      ~ (__ \ "hasFrontendRoutes").write[FrontendRoutes]
      ~ (__ \ "hasAppConfigBase" ).write[StatusCheck]
      ~ (__ \ "hasAppConfigEnv"  ).write[AppConfigEnvironment]
      ~ (__ \ "isDeployedIn"     ).write[DeploymentEnvironment]
      ~ (__ \ "hasDashboards"    ).write[Dashboard]
      ~ (__ \ "hasBuildJobs"     ).write[StatusCheck]
      ~ (__ \ "hasAlerts"        ).write[StatusCheck]
      )(unlift(ServiceCommissioningStatus.unapply))
}