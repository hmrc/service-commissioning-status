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

import play.api.Logging
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.servicecommissioningstatus.model.Environment

import java.net.URL

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

case class ShutterApiResponse(
  serviceName: String,
  environment: String,
  outagePageURL: String,
  warnings: Seq[ShutterApiWarningResponse],
  templatedElements: Seq[TemplatedContent]
)
object ShutterApiResponse {
  implicit val format: OFormat[ShutterApiResponse] = Json.format[ShutterApiResponse]
}

case class ShutterApiWarningResponse(
  `type`: String,
  message: String,
  consequence: String,
)
object ShutterApiWarningResponse {
  implicit val format: OFormat[ShutterApiWarningResponse] = Json.format[ShutterApiWarningResponse]
}

case class TemplatedContent(elementID: String, innerHTML: String)
object TemplatedContent {
  implicit val format: OFormat[TemplatedContent] = Json.format[TemplatedContent]
}

class ShutterApiConnector @Inject()(
  httpClientV2  : HttpClientV2,
  servicesConfig: ServicesConfig
)(implicit
  ec: ExecutionContext
) extends Logging {
  import HttpReads.Implicits._

  private lazy val shutterApiURL = s"${servicesConfig.baseUrl("shutter-api")}/shutter-api"

  def getShutterPage(serviceName: String, environment: Environment)(implicit hc: HeaderCarrier): Future[ShutterApiResponse] =
    httpClientV2
      .get(new URL(s"$shutterApiURL/${environment.asString.toLowerCase}/outage-pages/$serviceName"))
      .execute[ShutterApiResponse]
}
