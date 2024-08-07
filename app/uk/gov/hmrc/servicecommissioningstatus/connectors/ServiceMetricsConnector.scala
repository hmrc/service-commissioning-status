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

package uk.gov.hmrc.servicecommissioningstatus.connectors

import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, StringContextOps}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.servicecommissioningstatus.{Environment, ServiceName}

import java.time.LocalDate
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

object ServiceMetricsConnector:
  case class MongoCollectionSize(
    database: String
  , collection: String
  , sizeBytes: BigDecimal
  , date: LocalDate
  , environment: Environment
  , service: Option[String]
  )

  object MongoCollectionSize:
    val reads: Reads[MongoCollectionSize] =
      given Reads[Environment] = Environment.format
      ( (__ \ "database"   ).read[String]
      ~ (__ \ "collection" ).read[String]
      ~ (__ \ "sizeBytes"  ).read[BigDecimal]
      ~ (__ \ "date"       ).read[LocalDate]
      ~ (__ \ "environment").read[Environment]
      ~ (__ \ "service"    ).readNullable[String]
      )(apply _)

@Singleton
class ServiceMetricsConnector @Inject()(
  servicesConfig: ServicesConfig
, httpClientV2: HttpClientV2
)(using ExecutionContext):
  import HttpReads.Implicits._
  import ServiceMetricsConnector._

  private val url: String = servicesConfig.baseUrl("service-metrics")

  def getCollections(serviceName: ServiceName)(using HeaderCarrier): Future[Seq[MongoCollectionSize]] =
    given Reads[MongoCollectionSize] = MongoCollectionSize.reads

    httpClientV2
      .get(url"$url/service-metrics/${serviceName.asString}/collections")
      .execute[Seq[MongoCollectionSize]]
