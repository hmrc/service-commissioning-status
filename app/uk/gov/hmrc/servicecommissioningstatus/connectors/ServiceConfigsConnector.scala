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

import play.api.libs.json.Reads
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, StringContextOps}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.servicecommissioningstatus.{Environment, Parser, ServiceName}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

object ServiceConfigsConnector:
  import play.api.libs.functional.syntax._
  import play.api.libs.json._


  enum RouteType(val asString: String):
    case AdminFrontend extends RouteType("adminfrontend")
    case Frontend      extends RouteType("frontend")
  
  case class Route(
    environment         : Environment
  , ruleConfigurationUrl: String
  )

  object Route:
    val reads: Reads[Route] =
      ( (__ \ "environment"         ).read[Environment](Environment.format)
      ~ (__ \ "ruleConfigurationUrl").read[String]
      )(Route.apply _)

  case class AdminFrontendRoute(
    allow   : Map[Environment, List[String]]
  , location: String
  )

  object AdminFrontendRoute:
    val reads: Reads[AdminFrontendRoute] =
      import cats.implicits._
      given Reads[Map[Environment, List[String]]] =
        _.as[Map[String, List[String]]]
         .toList
         .traverse { case (k, v) => Parser[Environment].parse(k).map(_ -> v) }
         .fold(JsError.apply, xs => JsSuccess.apply(xs.toMap))

      ( (__ \ "allow"   ).read[Map[Environment, List[String]]]
      ~ (__ \ "location").read[String]
      )(AdminFrontendRoute.apply _)

  case class InternalAuthConfig(service: ServiceName, environment: Environment)

  object InternalAuthConfig:
    given reads: Reads[InternalAuthConfig] =
      ( (__ \ "serviceName").read[ServiceName](ServiceName.format)
      ~ (__ \ "environment").read[Environment](Environment.format)
      )(InternalAuthConfig.apply _)

class ServiceConfigsConnector @Inject()(
  servicesConfig: ServicesConfig,
  httpClientV2: HttpClientV2
)(implicit ec: ExecutionContext):
  import HttpReads.Implicits._
  import ServiceConfigsConnector._

  private val url: String = servicesConfig.baseUrl("service-configs")
  
  private given Reads[Route] = Route.reads
  
  def getMDTPFrontendRoutes(serviceName: ServiceName)(using HeaderCarrier): Future[Seq[Route]] =
    httpClientV2
      .get(url"$url/service-configs/routes?serviceName=${serviceName.asString}&routeType=frontend")
      .execute[Seq[Route]]
  
  def getAdminFrontendRoutes(serviceName: ServiceName)(using HeaderCarrier): Future[Seq[Route]] =
    httpClientV2
      .get(url"$url/service-configs/routes?serviceName=${serviceName.asString}&routeType=adminfrontend")
      .execute[Seq[Route]]

  private given Reads[InternalAuthConfig] = InternalAuthConfig.reads
  def getInternalAuthConfig(serviceName: ServiceName)(using HeaderCarrier): Future[Seq[InternalAuthConfig]] =
    httpClientV2
      .get(url"$url/service-configs/internal-auth-config/${serviceName.asString}")
      .execute[Seq[InternalAuthConfig]]

  def getConfigLocation(serviceName: ServiceName)(using HeaderCarrier): Future[Map[String, String]] =
    httpClientV2
      .get(url"$url/service-configs/services/${serviceName.asString}/config-location")
      .execute[Map[String, String]]
