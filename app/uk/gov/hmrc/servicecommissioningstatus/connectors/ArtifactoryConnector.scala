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


import play.api.libs.ws.WSClient
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.servicecommissioningstatus.config.ArtifactoryConfig
import akka.stream.Materializer
import akka.stream.scaladsl.StreamConverters


import java.io.InputStream
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ArtifactoryConnector @Inject()(
  config: ArtifactoryConfig,
  httpClientV2: HttpClientV2,
  ws    : WSClient
)(implicit ec: ExecutionContext,
  materializer: Materializer
){

  def getSensuZip: Future[InputStream] =
    ws
      .url(s"${config.artifactoryUrl}/artifactory/webstore/sensu-config/output.zip")
      .withMethod("GET")
      .withRequestTimeout(Duration.Inf)
      .stream()
      .map(_.bodyAsSource.async.runWith(StreamConverters.asInputStream(readTimeout = 20.seconds)))

//  def getSensuZip =
//    httpClientV2
//      .get(url"${config.artifactoryUrl}/artifactory/webstore/sensu-config/output.zip")
//      .stream
}
