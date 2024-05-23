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

package uk.gov.hmrc.servicecommissioningstatus.controllers

import play.api.Logging
import play.api.libs.json.{Format, Json, JsString, Reads, Writes, __}
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import uk.gov.hmrc.servicecommissioningstatus.{Check, LifecycleStatus, ServiceName, ServiceType, TeamName}
import uk.gov.hmrc.servicecommissioningstatus.persistence.CacheRepository.ServiceCheck
import uk.gov.hmrc.servicecommissioningstatus.persistence.LifecycleStatusRepository.Lifecycle
import uk.gov.hmrc.servicecommissioningstatus.service.StatusCheckService

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton()
class LifecycleStatusController @Inject()(
 cc                : ControllerComponents,
 statusCheckService: StatusCheckService
)(implicit
  ec: ExecutionContext
) extends BackendController(cc)
     with Logging {

  import LifecycleStatusController._

  def listAllChecks(): Action[AnyContent] = Action.apply {
    Ok(Json.toJson(statusCheckService.listAllChecks().flatMap {
      case (title, cls) if cls == classOf[Check.EnvCheck]    => Some(Json.obj(title -> JsString("environment")))
      case (title, cls) if cls == classOf[Check.SimpleCheck] => Some(Json.obj(title -> JsString("simple"     )))
      case _                                                 => None
    }))
  }

  def cachedStatusChecks(
    teamName       : Option[TeamName],
    serviceType    : Option[ServiceType],
    lifecycleStatus: List[LifecycleStatus]
  ) =
    Action.async { implicit request =>
      implicit val serviceCheckFormats: Writes[ServiceCheck] = ServiceCheck.format
      statusCheckService
        .cachedCommissioningStatusChecks(teamName, serviceType, lifecycleStatus)
        .map(results => Ok(Json.toJson(results)))
    }

  def statusChecks(serviceName: ServiceName): Action[AnyContent] =
    Action.async { implicit request =>
      implicit val checkFormats: Writes[Check] = Check.format
      statusCheckService
        .commissioningStatusChecks(serviceName)
        .map(results => Ok(Json.toJson(results)))
    }

  def getLifecycleStatus(serviceName: ServiceName): Action[AnyContent] =
    Action.async { implicit request =>
      statusCheckService
        .getLifecycleStatus(serviceName)
        .map(_.fold(NotFound(""))(result => Ok(Json.toJson(result)(Lifecycle.format))))
    }

  def setLifecycleStatus(serviceName: ServiceName): Action[LifecycleStatusRequest] =
    Action.async(parse.json[LifecycleStatusRequest](reads)) { implicit request =>
      if (request.body.lifecycleStatus == LifecycleStatus.DecommissionInProgress)
        statusCheckService
          .setLifecycleStatus(serviceName, request.body.lifecycleStatus, request.body.username)
          .map(_ => NoContent)
      else
        Future.successful(BadRequest("Unsupported service status - must be: 'DecommissionInProgress'."))
    }
}

object LifecycleStatusController {
  import play.api.libs.functional.syntax._

  case class LifecycleStatusRequest(
    lifecycleStatus: LifecycleStatus,
    username       : String
  )

  val reads: Reads[LifecycleStatusRequest] = {
    implicit val lsf: Format[LifecycleStatus] = LifecycleStatus.format
    ( (__ \ "lifecycleStatus").read[LifecycleStatus]
    ~ (__ \ "username"       ).read[String]
    )(LifecycleStatusRequest.apply _)
  }
}
