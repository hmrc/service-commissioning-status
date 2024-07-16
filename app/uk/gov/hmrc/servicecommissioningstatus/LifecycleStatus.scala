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

import play.api.libs.json.*
import play.api.mvc.{PathBindable, QueryStringBindable}
import FromStringEnum._

enum LifecycleStatus(val asString: String) extends FromString derives PathBindable, QueryStringBindable:
  case Active                 extends LifecycleStatus("Active")
  case Archived               extends LifecycleStatus("Archived")
  case DecommissionInProgress extends LifecycleStatus("DecommissionInProgress")
  case Deprecated             extends LifecycleStatus("Deprecated")
  case Deleted                extends LifecycleStatus("Deleted")

object LifecycleStatus:

  given Parser[LifecycleStatus] = Parser.parser(LifecycleStatus.values)
  
  val format: Format[LifecycleStatus] =
    new Format[LifecycleStatus]:
      override def reads(json: JsValue): JsResult[LifecycleStatus] =
        json match
          case JsString(s) => Parser[LifecycleStatus].parse(s).fold(msg => JsError(msg), rt => JsSuccess(rt))
          case _           => JsError("String value expected")

      override def writes(rt: LifecycleStatus): JsValue =
        JsString(rt.asString)

