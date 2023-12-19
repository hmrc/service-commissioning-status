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
import play.api.libs.json.{Json, JsString, Writes}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import uk.gov.hmrc.servicecommissioningstatus.{Check, TeamName, ServiceName, ServiceType}
import uk.gov.hmrc.servicecommissioningstatus.service.StatusCheckService
import uk.gov.hmrc.servicecommissioningstatus.persistence.CacheRepository.ServiceCheck
import uk.gov.hmrc.servicecommissioningstatus.persistence.LifeCycleStatusRepository.LifeCycleStatusType
import play.api.libs.json.{__, Reads}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton()
class ServiceStatusController @Inject()(
 cc: ControllerComponents,
 statusCheckService: StatusCheckService
)(implicit
  ec: ExecutionContext)
  extends BackendController(cc)
  with Logging {

  import ServiceStatusController._

  def listAllChecks(): Action[AnyContent] = Action.apply {
    Ok(Json.toJson(statusCheckService.listAllChecks().flatMap {
      case (title, cls) if cls == classOf[Check.EnvCheck]    => Some(Json.obj(title -> JsString("environment")))
      case (title, cls) if cls == classOf[Check.SimpleCheck] => Some(Json.obj(title -> JsString("simple"     )))
      case _                                                 => None
    }))
  }

  def cachedStatusChecks(teamName: Option[TeamName], serviceType: Option[ServiceType]) = Action.async { implicit request =>
    implicit val serviceCheckFormats: Writes[ServiceCheck] = ServiceCheck.format
    statusCheckService
      .cachedCommissioningStatusChecks(teamName, serviceType)
      .map(results => Ok(Json.toJson(results)))
  }

  def statusChecks(serviceName: ServiceName): Action[AnyContent] = Action.async { implicit request =>
    implicit val checkFormats: Writes[Check] = Check.format
    statusCheckService
      .commissioningStatusChecks(serviceName)
      .map(results => Ok(Json.toJson(results)))
  }

  def lifeCycleStatus(serviceName: ServiceName): Action[AnyContent] = Action.async { implicit request =>
    statusCheckService
      .lifeCycleStatus(serviceName)
      .map(_.fold(NotFound(""))(result => Ok(Json.toJson(result))))
  }

  def setLifeCycleStatus(serviceName: ServiceName): Action[setLifeCycleStatusRequest] = Action.async(parse.json[setLifeCycleStatusRequest](reads)) { implicit request =>
    val status = request.body.status
    if(status == LifeCycleStatusType.DecommissionInProgress)
      statusCheckService
        .setLifeCycleStatus(serviceName, status)
        .map(_ => NoContent)
    else
      Future.successful(BadRequest("Unsupported status type. Allowed values: 'DecommissionInProgress'."))
  }
}

object ServiceStatusController {
  case class setLifeCycleStatusRequest(status: LifeCycleStatusType)

  private implicit val lifeCycleStatusTypeFormat: play.api.libs.json.Format[LifeCycleStatusType] = LifeCycleStatusType.format
  val reads: Reads[setLifeCycleStatusRequest] =
   (__ \ "status").read[LifeCycleStatusType].map(setLifeCycleStatusRequest.apply)
}
