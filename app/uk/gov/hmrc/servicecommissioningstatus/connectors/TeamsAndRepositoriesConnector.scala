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

import play.api.Logger
import play.api.libs.json.{Reads, __}
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, StringContextOps}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.servicecommissioningstatus.{Enum, WithAsString, ServiceType, TeamName}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

object TeamsAndRepositoriesConnector {
  import play.api.libs.functional.syntax._
  import play.api.libs.json._

  sealed trait Tag extends WithAsString

  object Tag extends Enum[Tag] {
    case object AdminFrontend extends Tag { def asString = "admin" }
    case object Api           extends Tag { def asString = "api"   }
    case object Stub          extends Tag { def asString = "stub"  }

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

  sealed trait BuildJobType { def asString: String }

  object BuildJobType {
    case object Job         extends BuildJobType { override val asString = "job"         }
    case object Pipeline    extends BuildJobType { override val asString = "pipeline"    }
    case object Performance extends BuildJobType { override val asString = "performance" }

    private val logger = Logger(this.getClass)

    val values: List[BuildJobType] =
      List(Job, Pipeline, Performance)

    def parse(s: String): BuildJobType =
      values
        .find(_.asString.equalsIgnoreCase(s)).getOrElse {
        logger.info(s"Unable to find job type: $s, defaulted to: job")
        Job
      }

    implicit val reads: Reads[BuildJobType] =
      Reads.of[String].map(parse)
  }

  case class BuildJob(
    repoName   : String,
    jenkinsUrl : String,
    jobType    : BuildJobType
  )

  object BuildJob {
    implicit val reads: Reads[BuildJob] =
      ( (__ \ "repoName"  ).read[String]
      ~ (__ \ "jenkinsURL").read[String]
      ~ (__ \ "jobType"   ).read[BuildJobType]
      )(BuildJob.apply _)
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
  def findServiceRepos(
    name       : Option[String]      = None
  , team       : Option[TeamName]    = None
  , serviceType: Option[ServiceType] = None
  )(implicit hc: HeaderCarrier): Future[Seq[Repo]] =
    httpClientV2
      .get(url"$url/api/v2/repositories?repoType=service&name=$name&team=${team.map(_.asString)}&serviceType=${serviceType.map(_.asString)}")
      .execute[Seq[Repo]]

  def findBuildJobs(repoName: String)(implicit hc: HeaderCarrier): Future[Seq[BuildJob]] = {
    implicit val readJobs: Reads[Seq[BuildJob]] =
      Reads.at(__ \ "jobs")(Reads.seq(BuildJob.reads))

    httpClientV2
      .get(url"$url/api/jenkins-jobs/$repoName")
      .execute[Seq[BuildJob]]
  }
}
