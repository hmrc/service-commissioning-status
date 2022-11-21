/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.libs.json.{Reads, __}
import uk.gov.hmrc.http.{HeaderCarrier, StringContextOps}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ServiceConfigsConnector @Inject()(
  servicesConfig: ServicesConfig,
  httpClientV2: HttpClientV2
)(implicit ec: ExecutionContext){

  private val url: String = servicesConfig.baseUrl("service-configs")

  def getMDTPFrontendRoutes(serviceName: String)(implicit hc:  HeaderCarrier): Future[Seq[FrontendRoute]] = {
    implicit val r: Reads[FrontendRoute] = FrontendRoute.reads
    httpClientV2
      .get(url"$url/frontend-route/$serviceName")
      .execute[Seq[FrontendRoute]]
  }
}

case class Routes(ruleConfigurationUrl: String)

object Routes {
  val reads: Reads[Routes] =
    (__ \ "ruleConfigurationUrl").read[String].map(Routes(_))
}

case class FrontendRoute(environment: String, routes: Seq[Routes])

object FrontendRoute {
  val reads: Reads[FrontendRoute] = {
    implicit val rs: Reads[Routes] = Routes.reads
    ( (__ \ "environment").read[String]
      ~ (__ \ "routes"   ).read[Seq[Routes]]
    )(FrontendRoute.apply _)
  }
}

