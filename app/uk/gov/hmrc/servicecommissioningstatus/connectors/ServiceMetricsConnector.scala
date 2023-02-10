package uk.gov.hmrc.servicecommissioningstatus.connectors

import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._
import uk.gov.hmrc.http.{HeaderCarrier, StringContextOps}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.servicecommissioningstatus.connectors.ServiceMetricsConnector.MongoCollectionSize
import uk.gov.hmrc.servicecommissioningstatus.model.Environment

import java.time.LocalDate
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

object ServiceMetricsConnector {
  case class MongoCollectionSize(
    database: String
  , collection: String
  , sizeBytes: BigDecimal
  , date: LocalDate
  , environment: Environment
  , service: Option[String]
  )

  object MongoCollectionSize {
    val reads: Reads[MongoCollectionSize] = {
      implicit val envf = Environment.format
      ( (__ \ "database"   ).read[String]
      ~ (__ \ "collection" ).read[String]
      ~ (__ \ "sizeBytes"  ).read[BigDecimal]
      ~ (__ \ "date"       ).read[LocalDate]
      ~ (__ \ "environment").read[Environment]
      ~ (__ \ "service"    ).readNullable[String]
      )(apply _)
    }
  }
}

@Singleton
class ServiceMetricsConnector @Inject()(
  servicesConfig: ServicesConfig
, httpClientV2: HttpClientV2
)(implicit
  ec: ExecutionContext
){

  private val url: String = servicesConfig.baseUrl("service-metrics")

  def getCollections(service: String)(implicit hc: HeaderCarrier): Future[Seq[MongoCollectionSize]] = {
    implicit val mcsR = MongoCollectionSize.reads

    httpClientV2
      .get(url"$url/service-metrics/$service/collections")
      .execute[Seq[MongoCollectionSize]]
  }
}
