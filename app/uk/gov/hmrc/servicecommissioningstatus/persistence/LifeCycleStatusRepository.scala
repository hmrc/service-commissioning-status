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

package uk.gov.hmrc.servicecommissioningstatus.persistence

import org.mongodb.scala.model.{Filters, FindOneAndReplaceOptions, Indexes, IndexModel, IndexOptions, Sorts}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}
import uk.gov.hmrc.servicecommissioningstatus.{ServiceName, LifecycleStatus}

import javax.inject.{Inject, Singleton}
import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LifecycleStatusRepository @Inject()(
  val mongoComponent: MongoComponent
)(implicit
  ec: ExecutionContext
) extends PlayMongoRepository[LifecycleStatusRepository.Row](
  mongoComponent = mongoComponent,
  collectionName = "lifecycleStatus",
  domainFormat   = LifecycleStatusRepository.LifecycleStatusRow.format,
  indexes        = Seq(
                      IndexModel(Indexes.ascending("serviceName") , IndexOptions().background(true).unique(true).name("serviceNameIdx")),
                      IndexModel(Indexes.descending("createdDate"), IndexOptions().name("createdDateIdx")),
                   ),
  extraCodecs    = Seq(Codecs.playFormatCodec(ServiceName.format))
) {

  override lazy val requiresTtlIndex = false

  def setLifecycleStatus(serviceName: ServiceName, lifecycleStatus: LifecycleStatus, username: String): Future[Unit] =
    collection
      .findOneAndReplace(
        filter      = Filters.equal("serviceName", serviceName),
        replacement = LifecycleStatusRepository.Row(serviceName, lifecycleStatus, username, Instant.now),
        options     = FindOneAndReplaceOptions().upsert(true)
      )
      .toFuture()
      .map(_ => ())

  def lastLifecycleStatus(serviceName: ServiceName): Future[Option[LifecycleStatus]] =
    collection
      .find(Filters.eq("serviceName", serviceName))
      .sort(Sorts.orderBy(Sorts.descending("createDate")))
      .limit(1)
      .headOption()
      .map(_.map(_.lifecycleStatus))
}

object LifecycleStatusRepository {
  import play.api.libs.functional.syntax._
  import play.api.libs.json._

  case class Row(
    serviceName    : ServiceName
  , lifecycleStatus: LifecycleStatus
  , username       : String
  , createdDate    : Instant
  )

  object LifecycleStatusRow {

    val format: Format[Row] =
      ( (__ \ "serviceName"    ).format[ServiceName](ServiceName.format)
      ~ (__ \ "lifecycleStatus").format[LifecycleStatus](LifecycleStatus.format)
      ~ (__ \ "username"       ).format[String]
      ~ (__ \ "createDate"     ).format[Instant]
      )(Row.apply, unlift(Row.unapply))
  }
}
