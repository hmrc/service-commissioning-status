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

package uk.gov.hmrc.servicecommissioningstatus.connectors.model

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsError, JsSuccess, Reads, __}
import uk.gov.hmrc.servicecommissioningstatus.model.Environment

case class InternalAuthConfig(service: ServiceName, environment: Environment, grantType: GrantType)

object InternalAuthConfig {
  implicit val reads: Reads[InternalAuthConfig] = {
    implicit val environmentReads = Environment.reads
    ((__ \ "serviceName").read[ServiceName]
      ~ (__ \ "environment").read[Environment]
      ~ (__ \ "grantType").read[GrantType]
      )(InternalAuthConfig.apply _)
  }
}


case class ServiceName(serviceName: String) extends AnyVal

object ServiceName {
  implicit val reads: Reads[ServiceName] = __.read[String].map(ServiceName(_))
}

sealed trait GrantType{ def asString: String }

object GrantType {

  case object Grantee extends GrantType {
    val asString = "grantee"
  }

  case object Grantor extends GrantType {
    val asString = "grantor"
  }

  implicit val reads: Reads[GrantType] =
    _.validate[String].flatMap {
      case "grantee" => JsSuccess(Grantee)
      case "grantor" => JsSuccess(Grantor)
      case _ => JsError("Invalid Grant Type")
    }
}