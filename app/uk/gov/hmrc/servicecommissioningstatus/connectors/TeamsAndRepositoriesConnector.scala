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
import uk.gov.hmrc.servicecommissioningstatus.model.{Enum, WithAsString}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

object TeamsAndRepositoriesConnector {
  import play.api.libs.functional.syntax._
  import play.api.libs.json._

  sealed trait ServiceType extends WithAsString
  object ServiceType extends Enum[ServiceType] {
    case object Frontend extends ServiceType { def asString = "FrontendService" }
    case object Backend extends ServiceType  { def asString = "BackendService"  }

    override val values = List(Frontend, Backend)
  }

  sealed trait Tag extends WithAsString

  object Tag extends Enum[Tag] {
    case object AdminFrontend extends Tag { def asString = "AdminFrontend" }
    case object Api           extends Tag { def asString = "Api"           }
    case object Stub          extends Tag { def asString = "Stub"          }

    override val values = List(AdminFrontend, Api, Stub)
  }

  case class Repo(
    name       : String
  , serviceType: Option[ServiceType] = None
  , tags       : Seq[Tag] = Seq.empty
  , isArchived : Boolean
  , githubUrl  : String
  )

  object Repo {
    val reads: Reads[Repo] = {
      implicit val readServiceType = ServiceType.reads
      implicit val readTag         = Tag.reads
      ( (__ \ "name"       ).read[String]
      ~ (__ \ "serviceType").readNullable[ServiceType]
      ~ (__ \ "tags"       ).readWithDefault[Seq[Tag]](Seq.empty)
      ~ (__ \ "isArchived" ).readWithDefault[Boolean](false)
      ~ (__ \ "url"        ).read[String]
      ) (apply _)
    }
  }
}

@Singleton
class TeamsAndRepositoriesConnector @Inject()(
  httpClientV2  : HttpClientV2,
  servicesConfig: ServicesConfig
)(implicit val ec: ExecutionContext) {
  import HttpReads.Implicits._
  import TeamsAndRepositoriesConnector._

  private val url = servicesConfig.baseUrl("teams-and-repositories")

  private implicit val readRepo = Repo.reads
  def findRepo(name: String)(implicit hc: HeaderCarrier): Future[Option[Repo]] =
    httpClientV2
      .get(url"$url/api/v2/repositories/$name")
      .execute[Option[Repo]]
}
