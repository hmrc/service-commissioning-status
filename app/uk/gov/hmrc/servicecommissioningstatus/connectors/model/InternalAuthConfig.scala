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
import play.api.libs.json.{Reads, __}
import uk.gov.hmrc.servicecommissioningstatus.model.Environment

case class InternalAuthConfig(service: ServiceName, environment: Environment)

object InternalAuthConfig {
  implicit val reads: Reads[InternalAuthConfig] = {
    implicit val environmentReads = Environment.reads
    ((__ \ "serviceName").read[ServiceName]
      ~ (__ \ "environment").read[Environment]
      )(InternalAuthConfig.apply _)
  }
}


case class ServiceName(serviceName: String) extends AnyVal

object ServiceName {
  implicit val reads: Reads[ServiceName] = __.read[String].map(ServiceName(_))
}