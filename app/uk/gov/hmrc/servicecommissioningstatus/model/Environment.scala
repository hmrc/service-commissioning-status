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

package uk.gov.hmrc.servicecommissioningstatus.model

import uk.gov.hmrc.servicecommissioningstatus.model.Environment.{Production, QA}

sealed trait Environment extends WithAsString {
  def toInternalAuthEnv: Option[String] = this match {
    case Production => Some("prod")
    case QA => Some(this.asString)
    case _ => None
  }

}

object Environment extends Enum[Environment] {
  case object Development  extends Environment { val asString = "development" }
  case object Integration  extends Environment { val asString = "integration" }
  case object QA           extends Environment { val asString = "qa"          }
  case object Staging      extends Environment { val asString = "staging"     }
  case object ExternalTest extends Environment { val asString = "externaltest"}
  case object Production   extends Environment { val asString = "production"  }

  override val values: List[Environment] =
    List(Development, Integration, QA, Staging, ExternalTest, Production)

  val internalAuthEnvs: List[Environment] = List(Production, QA)
}
