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

import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.servicecommissioningstatus.config.ArtifactoryConfig
import akka.stream.Materializer
import akka.stream.scaladsl.{Source, StreamConverters}
import akka.util.ByteString
import uk.gov.hmrc.http.{HeaderCarrier, StringContextOps, UpstreamErrorResponse}

import java.io.InputStream
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ArtifactoryConnector @Inject()(
  config: ArtifactoryConfig,
  httpClientV2: HttpClientV2
)(implicit ec: ExecutionContext,
  materializer: Materializer
){

  def getSensuZip(implicit hc: HeaderCarrier): Future[Option[InputStream]] = {
    httpClientV2
      .get(url"${config.artifactoryUrl}/artifactory/webstore/sensu-config/output.zip")
      .withProxy
      .stream[Either[UpstreamErrorResponse, Source[ByteString, _]]]
      .flatMap{
        case Right(source)                                   => Future.successful(Some(source.runWith(StreamConverters.asInputStream())))
        case Left(UpstreamErrorResponse.WithStatusCode(404)) => Future.successful(None)
        case Left(error)                                     => throw error
      }
  }
}
