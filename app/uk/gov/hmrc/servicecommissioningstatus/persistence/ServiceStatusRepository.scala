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
class ServiceStatusRepository @Inject()(
  val mongoComponent: MongoComponent
)(implicit
  ec: ExecutionContext
) extends PlayMongoRepository[ServiceStatusRepository.ServiceStatus](
  mongoComponent = mongoComponent,
  collectionName = "serviceStatus",
  domainFormat   = ServiceStatusRepository.ServiceStatus.format,
  indexes        = Seq(
                      IndexModel(Indexes.ascending("serviceName"), IndexOptions().background(true).unique(true).name("serviceNameIdx")),
                      IndexModel(Indexes.descending("date"),IndexOptions().name("date")),
                   ),
  extraCodecs    = Seq(Codecs.playFormatCodec(ServiceName.format))
) {

  override lazy val requiresTtlIndex = false

  def setStatus(serviceName: ServiceName, status: ServiceStatusRepository.ServiceStatusType): Future[Unit] =
    collection
      .findOneAndReplace(
        filter      = Filters.equal("serviceName", serviceName),
        replacement = ServiceStatusRepository.ServiceStatus(serviceName, status, Instant.now),
        options     = FindOneAndReplaceOptions().upsert(true)
      )
      .toFuture()
      .map(_ => ())

  def status(serviceName: ServiceName): Future[Option[ServiceStatusRepository.ServiceStatus]] =
    collection.find(Filters.eq("serviceName", serviceName))
      .sort(Sorts.orderBy(Sorts.descending("date")))
      .limit(1)
      .headOption()
}

object ServiceStatusRepository {
  import play.api.libs.functional.syntax._
  import play.api.libs.json._

  sealed trait ServiceStatusType { val asString: String }

  object ServiceStatusType {
    object BeingDecommissioned extends ServiceStatusType { val asString: String = "BeingDecommissioned" }
    object Archived            extends ServiceStatusType { val asString: String = "Archived" }
    object Deprecated          extends ServiceStatusType { val asString: String = "Deprecated" }
    object Active              extends ServiceStatusType { val asString: String = "Active" }

    val values: List[ServiceStatusType] = List(BeingDecommissioned, Archived, Deprecated, Active)

    def parse(s: String): Either[String, ServiceStatusType] =
      values
        .find(_.asString == s)
        .toRight(s"Invalid service status - should be one of: ${values.map(_.asString).mkString(", ")}")

    val format: Format[ServiceStatusType] =
      new Format[ServiceStatusType] {
        override def reads(json: JsValue): JsResult[ServiceStatusType] =
          json match {
            case JsString(s) => parse(s).fold(msg => JsError(msg), rt => JsSuccess(rt))
            case _           => JsError("String value expected")
          }

        override def writes(rt: ServiceStatusType): JsValue =
          JsString(rt.asString)
      }
  }

  case class ServiceStatus(
    serviceName: ServiceName
  , status     : ServiceStatusType
  , date       : Instant
  )

  object ServiceStatus {

    private implicit val serviceStatusTypeFormat: Format[ServiceStatusType] = ServiceStatusType.format
    val format: Format[ServiceStatus] =
      ( (__ \ "serviceName").format[ServiceName](ServiceName.format)
      ~ (__ \ "status"     ).format[ServiceStatusType]
      ~ (__ \ "date"       ).format[Instant]
      )(ServiceStatus.apply, unlift(ServiceStatus.unapply))

  }
}
