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

import play.api.libs.json.{Format, Json, OWrites, Writes}

import java.time.Instant

sealed trait Lifecycle {
  val status: LifecycleStatus
}

case class LifecycleWithMetaData(status: LifecycleStatus, createDate: Instant, username: String) extends Lifecycle

object LifecycleWithMetaData {
  implicit val lcsf: Format[LifecycleStatus] = LifecycleStatus.format
  val writes: OWrites[LifecycleWithMetaData] = Json.writes[LifecycleWithMetaData]
}

case class LifecycleWithoutMetaData(status: LifecycleStatus) extends Lifecycle

object LifecycleWithoutMetaData {
  implicit val lcsf: Format[LifecycleStatus] = LifecycleStatus.format
  val writes: OWrites[LifecycleWithoutMetaData] = Json.writes[LifecycleWithoutMetaData]
}

object Lifecycle {
  implicit val writes: Writes[Lifecycle] = {
    case lwmd: LifecycleWithMetaData     => Json.toJson(lwmd)(LifecycleWithMetaData.writes)
    case lwomd: LifecycleWithoutMetaData => Json.toJson(lwomd)(LifecycleWithoutMetaData.writes)
  }
}
