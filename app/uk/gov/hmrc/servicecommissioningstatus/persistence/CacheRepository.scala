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

import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.{Filters, Indexes, IndexModel, IndexOptions}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}
import uk.gov.hmrc.mongo.transaction.{TransactionConfiguration, Transactions}
import uk.gov.hmrc.servicecommissioningstatus.{Check, ServiceName}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CacheRepository @Inject()(
  override val mongoComponent: MongoComponent
)(implicit
  ec: ExecutionContext
) extends PlayMongoRepository[CacheRepository.ServiceCheck](
  mongoComponent = mongoComponent,
  collectionName = "cachedServiceCheck",
  domainFormat   = CacheRepository.ServiceCheck.format,
  indexes        = Seq(
                     IndexModel(Indexes.ascending("serviceName"), IndexOptions().background(true).name("serviceNameIdx"))
                   ),
  extraCodecs    = Seq(Codecs.playFormatCodec(ServiceName.format))
) with Transactions {

  // we replace all the data for each call to putAll
  override lazy val requiresTtlIndex = false

  private implicit val tc: TransactionConfiguration = TransactionConfiguration.strict

  def putAll(items: Seq[CacheRepository.ServiceCheck]): Future[Unit] =
    withSessionAndTransaction { session =>
      for {
        _ <- collection.deleteMany(session, BsonDocument()).toFuture()
        _ <- collection.insertMany(session, items).toFuture()
      } yield ()
    }

  def findAll(serviceNames: Seq[ServiceName]): Future[Seq[CacheRepository.ServiceCheck]] =
    collection
      .find(Filters.in("serviceName", serviceNames: _*))
      .toFuture()

}

object CacheRepository {
  import play.api.libs.functional.syntax._
  import play.api.libs.json._

  case class ServiceCheck(
    serviceName: ServiceName
  , checks     : Seq[Check]
  )

  object ServiceCheck {
    val format: Format[ServiceCheck] = {
      implicit val formatCheck   = Check.format
      ( (__ \ "serviceName").format[ServiceName](ServiceName.format)
      ~ (__ \ "checks"     ).format[Seq[Check]]
      )(ServiceCheck.apply, unlift(ServiceCheck.unapply))
    }
  }
}
