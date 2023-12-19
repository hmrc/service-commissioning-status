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
import uk.gov.hmrc.servicecommissioningstatus.ServiceName

import javax.inject.{Inject, Singleton}
import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LifeCycleStatusRepository @Inject()(
  val mongoComponent: MongoComponent
)(implicit
  ec: ExecutionContext
) extends PlayMongoRepository[LifeCycleStatusRepository.LifeCycleStatus](
  mongoComponent = mongoComponent,
  collectionName = "LifeCycleStatus",
  domainFormat   = LifeCycleStatusRepository.LifeCycleStatus.format,
  indexes        = Seq(
                      IndexModel(Indexes.ascending("serviceName"), IndexOptions().background(true).unique(true).name("serviceNameIdx")),
                      IndexModel(Indexes.descending("date"),IndexOptions().name("date")),
                   ),
  extraCodecs    = Seq(Codecs.playFormatCodec(ServiceName.format))
) {

  override lazy val requiresTtlIndex = false

  def setLifeCycleStatus(serviceName: ServiceName, status: LifeCycleStatusRepository.LifeCycleStatusType): Future[Unit] =
    collection
      .findOneAndReplace(
        filter      = Filters.equal("serviceName", serviceName),
        replacement = LifeCycleStatusRepository.LifeCycleStatus(serviceName, status, Instant.now),
        options     = FindOneAndReplaceOptions().upsert(true)
      )
      .toFuture()
      .map(_ => ())

  def lifeCycleStatus(serviceName: ServiceName): Future[Option[LifeCycleStatusRepository.LifeCycleStatus]] =
    collection.find(Filters.eq("serviceName", serviceName))
      .sort(Sorts.orderBy(Sorts.descending("date")))
      .limit(1)
      .headOption()
}

object LifeCycleStatusRepository {
  import play.api.libs.functional.syntax._
  import play.api.libs.json._

  sealed trait LifeCycleStatusType { val asString: String }

  object LifeCycleStatusType {
    object DecommissionInProgress extends LifeCycleStatusType { val asString: String = "DecommissionInProgress" }
    object Archived               extends LifeCycleStatusType { val asString: String = "Archived" }
    object Deprecated             extends LifeCycleStatusType { val asString: String = "Deprecated" }
    object Active                 extends LifeCycleStatusType { val asString: String = "Active" }

    val values: List[LifeCycleStatusType] = List(DecommissionInProgress, Archived, Deprecated, Active)

    def parse(s: String): Either[String, LifeCycleStatusType] =
      values
        .find(_.asString == s)
        .toRight(s"Invalid service status - should be one of: ${values.map(_.asString).mkString(", ")}")

    val format: Format[LifeCycleStatusType] =
      new Format[LifeCycleStatusType] {
        override def reads(json: JsValue): JsResult[LifeCycleStatusType] =
          json match {
            case JsString(s) => parse(s).fold(msg => JsError(msg), rt => JsSuccess(rt))
            case _           => JsError("String value expected")
          }

        override def writes(rt: LifeCycleStatusType): JsValue =
          JsString(rt.asString)
      }
  }

  case class LifeCycleStatus(
    serviceName: ServiceName
  , status     : LifeCycleStatusType
  , date       : Instant
  )

  object LifeCycleStatus {

    private implicit val LifeCycleStatusTypeFormat: Format[LifeCycleStatusType] = LifeCycleStatusType.format
    val format: Format[LifeCycleStatus] =
      ( (__ \ "serviceName").format[ServiceName](ServiceName.format)
      ~ (__ \ "status"     ).format[LifeCycleStatusType]
      ~ (__ \ "date"       ).format[Instant]
      )(LifeCycleStatus.apply, unlift(LifeCycleStatus.unapply))

  }
}
