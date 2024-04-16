/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.servicecommissioningstatus

import play.api.libs.json._

sealed trait LifecycleStatus { val asString: String }

object LifecycleStatus {
  object Active                 extends LifecycleStatus { val asString: String = "Active" }
  object Archived               extends LifecycleStatus { val asString: String = "Archived" }
  object DecommissionInProgress extends LifecycleStatus { val asString: String = "DecommissionInProgress" }
  object Deprecated             extends LifecycleStatus { val asString: String = "Deprecated" }
  object Deleted                extends LifecycleStatus { val asString: String = "Deleted" }

  val values: List[LifecycleStatus] = List(Active, Archived, DecommissionInProgress, Deprecated, Deleted)

  def parse(s: String): Either[String, LifecycleStatus] =
    values
      .find(_.asString == s)
      .toRight(s"Invalid service status - should be one of: ${values.map(_.asString).mkString(", ")}")

  val format: Format[LifecycleStatus] =
    new Format[LifecycleStatus] {
      override def reads(json: JsValue): JsResult[LifecycleStatus] =
        json match {
          case JsString(s) => parse(s).fold(msg => JsError(msg), rt => JsSuccess(rt))
          case _           => JsError("String value expected")
        }

      override def writes(rt: LifecycleStatus): JsValue = {
        JsString(rt.asString)
      }
    }
}
