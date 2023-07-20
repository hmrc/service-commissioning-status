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

import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, StringContextOps}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.servicecommissioningstatus.model.{Check, Environment}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.servicecommissioningstatus.connectors.model.InternalAuthConfig

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

object ServiceConfigsConnector {
  case class Routes(ruleConfigurationUrl: String)

  object Routes {
    val reads: Reads[Routes] =
      (__ \ "ruleConfigurationUrl").read[String].map(Routes(_))
  }

  case class FrontendRoute(environment: Environment, routes: Seq[Routes])

  object FrontendRoute {
    val reads: Reads[FrontendRoute] = {
      implicit val readsRoute: Reads[Routes]    = Routes.reads
      implicit val envReads: Reads[Environment] = Environment.reads
      ( (__ \ "environment").read[Environment]
      ~ (__ \ "routes"     ).read[Seq[Routes]]
      )(FrontendRoute.apply _)
    }
  }

  case class AdminFrontendRoute(
    allow   : Map[Environment, List[String]]
  , location: String
  )

  object AdminFrontendRoute {
    val reads: Reads[AdminFrontendRoute] = {
      import cats.implicits._
      implicit val readsAllow: Reads[Map[Environment, List[String]]] =
        _.as[Map[String, List[String]]]
         .toList
         .traverse { case (k, v) => Environment.parse(k).map(_ -> v) }
         .fold(JsError.apply, xs => JsSuccess.apply(xs.toMap))

      ( (__ \ "allow"   ).read[Map[Environment, List[String]]]
      ~ (__ \ "location").read[String]
      )(AdminFrontendRoute.apply _)
    }
  }
}

class ServiceConfigsConnector @Inject()(
  servicesConfig: ServicesConfig,
  httpClientV2: HttpClientV2
)(implicit ec: ExecutionContext){
  import HttpReads.Implicits._
  import ServiceConfigsConnector._

  private val url: String = servicesConfig.baseUrl("service-configs")

  private implicit val readsEnv: Reads[Environment] = Environment.reads

  private implicit val frontendRoutesReads = FrontendRoute.reads
  def getMDTPFrontendRoutes(serviceName: String)(implicit hc: HeaderCarrier): Future[Seq[FrontendRoute]] =
    httpClientV2
      .get(url"$url/service-configs/frontend-route/$serviceName")
      .execute[Seq[FrontendRoute]]

  private implicit val adminFrontendRoutesReads = AdminFrontendRoute.reads
  def getAdminFrontendRoutes(serviceName: String)(implicit hc: HeaderCarrier): Future[Seq[AdminFrontendRoute]] =
    httpClientV2
      .get(url"$url/service-configs/admin-frontend-route/$serviceName")
      .execute[Seq[AdminFrontendRoute]]

  private implicit val internalAuthConfigFormat = InternalAuthConfig.reads
  def getInternalAuthConfig(serviceName: String)(implicit hc: HeaderCarrier): Future[Seq[InternalAuthConfig]] =
    httpClientV2
      .get(url"$url/service-configs/internal-auth-config/$serviceName")
      .execute[Seq[InternalAuthConfig]]

  def getGrafanaDashboard(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    httpClientV2
      .get(url"$url/service-configs/grafana-dashboards/$serviceName")
      .execute[Option[JsValue]]
      .map(_.map(js => (js \ "location").as[String]))
      .map {
        case Some(e) => Right(Check.Present(e))
        case None    => Left(Check.Missing(s"https://github.com/hmrc/grafana-dashboards"))
      }

  def getKibanaDashboard(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    httpClientV2
      .get(url"$url/service-configs/kibana-dashboards/$serviceName")
      .execute[Option[JsValue]]
      .map(_.map(js => (js \ "location").as[String]))
      .map {
        case Some(e) => Right(Check.Present(e))
        case None    => Left(Check.Missing(s"https://github.com/hmrc/kibana-dashboards"))
      }

  def getBuildJobs(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    httpClientV2
      .get(url"$url/service-configs/build-jobs/$serviceName")
      .execute[Option[JsValue]]
      .map(_.map(js => (js \ "location").as[String]))
      .map {
        case Some(e) => Right(Check.Present(e))
        case None    => Left(Check.Missing(s"https://github.com/hmrc/build-jobs"))
      }

  def getAlertConfig(serviceName: String)(implicit hc: HeaderCarrier): Future[Check.Result] =
    httpClientV2
      .get(url"$url/service-configs/alert-configs/$serviceName")
      .execute[Option[JsValue]]
      .map(_.map(js => (js \ "location").as[String]))
      .map {
        case Some(e) => Right(Check.Present(e))
        case None    => Left(Check.Missing(s"https://github.com/hmrc/alert-config"))
      }

  def getShutterPages(serviceName: String)(implicit hc: HeaderCarrier): Future[List[(Environment, Check.Result)]] =
    for {
      optOutagePagesEnvironments <- httpClientV2
                                      .get(url"$url/service-configs/outage-pages/$serviceName")
                                      .execute[Option[JsValue]]
                                      .map(_.map(_.as[Set[Environment]]))
      outagePagesUrl             = "https://github.com/hmrc/outage-pages"
    } yield {
      Environment.values.map{env =>
        val envOutagePageUrl = s"$outagePagesUrl/blob/main/${env.asString}"
        optOutagePagesEnvironments.flatMap(
          _.find(_ == env).map(_ => 
            (env, Right(Check.Present(s"$envOutagePageUrl/$serviceName/index.html")))
          )
        ).getOrElse((env, Left(Check.Missing(envOutagePageUrl))))
      }
  }
}
