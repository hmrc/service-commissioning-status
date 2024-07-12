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

import javax.inject.{Inject, Singleton}

import org.mongodb.scala.model.{Filters, Indexes, IndexModel, IndexOptions, Sorts}
import org.mongodb.scala.ObservableFuture

import scala.concurrent.{ExecutionContext, Future}

import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}
import uk.gov.hmrc.servicecommissioningstatus.{Check, LifecycleStatus, ServiceName, Warning}

@Singleton
class CacheRepository @Inject()(
  mongoComponent: MongoComponent
)(using ExecutionContext
) extends PlayMongoRepository[CacheRepository.ServiceCheck](
  mongoComponent = mongoComponent,
  collectionName = "cachedServiceCheck",
  domainFormat   = CacheRepository.ServiceCheck.format,
  indexes        = Seq(
                     IndexModel(Indexes.ascending("serviceName"  ), IndexOptions().name("serviceNameIdx"))
                   , IndexModel(Indexes.ascending("lifecycleStatus"), IndexOptions().name("lifecycleStatusIdx"))
                   ),
  extraCodecs    = Seq(Codecs.playFormatCodec(ServiceName.format)) ++
                     Codecs.playFormatSumCodecs(LifecycleStatus.format)
):
  import CacheRepository._

  // we replace all the data for each call to putAll
  override lazy val requiresTtlIndex = false

  def putAll(items: Seq[ServiceCheck]): Future[Unit] =
    MongoUtils.replace[ServiceCheck](
      collection    = collection,
      newVals       = items,
      compareById   = (a, b) => a.serviceName == b.serviceName,
      filterById    = entry => Filters.equal("serviceName", entry.serviceName)
    )

  def findAll(serviceNames: Seq[ServiceName], lifecycleStatus: Seq[LifecycleStatus]): Future[Seq[ServiceCheck]] =
    collection
      .find(
        Seq(
          Option.when(serviceNames.nonEmpty)(Filters.in("serviceName", serviceNames: _*)),
          Option.when(lifecycleStatus.nonEmpty)(Filters.in("lifecycleStatus", lifecycleStatus: _*))
        ).flatten
         .foldLeft(Filters.empty())(Filters.and(_, _))
      ).sort(Sorts.ascending("serviceName"))
      .toFuture()

object CacheRepository:
  import play.api.libs.functional.syntax._
  import play.api.libs.json._

  case class ServiceCheck(
    serviceName    : ServiceName
  , lifecycleStatus: LifecycleStatus
  , checks         : Seq[Check]
  , warnings       : Option[Seq[Warning]]
  )

  object ServiceCheck {
    val format: Format[ServiceCheck] =
      given Format[Check] = Check.format
      given OFormat[Warning] = Warning.format
      ( (__ \ "serviceName"    ).format[ServiceName](ServiceName.format)
      ~ (__ \ "lifecycleStatus").format[LifecycleStatus](LifecycleStatus.format)
      ~ (__ \ "checks"         ).format[Seq[Check]]
      ~ (__ \ "warnings"       ).formatNullable[Seq[Warning]]
      )(ServiceCheck.apply, s => Tuple.fromProductTyped(s))
  }
