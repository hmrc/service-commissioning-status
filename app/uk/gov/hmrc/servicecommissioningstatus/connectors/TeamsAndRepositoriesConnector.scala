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
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.servicecommissioningstatus.RepoType.Service
import uk.gov.hmrc.servicecommissioningstatus.{Enum, RepoType, ServiceName, ServiceType, TeamName, WithAsString}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

object TeamsAndRepositoriesConnector {
  import play.api.libs.functional.syntax._
  import play.api.libs.json._

  sealed trait Tag extends WithAsString

  object Tag extends Enum[Tag] {
    case object AdminFrontend    extends Tag { def asString = "admin"              }
    case object Api              extends Tag { def asString = "api"                }
    case object BuiltOffPlatform extends Tag { def asString = "built-off-platform" }
    case object Maven            extends Tag { def asString = "maven"              }
    case object Stub             extends Tag { def asString = "stub"               }

    override val values = List(AdminFrontend, Api, BuiltOffPlatform, Maven, Stub)
  }

  case class Repo(
    name        : String
  , serviceType : Option[ServiceType] = None
  , tags        : Seq[Tag] = Seq.empty
  , isArchived  : Boolean
  , isDeprecated: Boolean
  , githubUrl   : String
  , isDeleted   : Boolean
  , teamNames   : Seq[String]
  )

  object Repo {
    val readsActive: Reads[Repo] = {
      implicit val readServiceType = ServiceType.reads
      implicit val readTag         = Tag.reads
      ( (__ \ "name"        ).read[String]
      ~ (__ \ "serviceType" ).readNullable[ServiceType]
      ~ (__ \ "tags"        ).readWithDefault[Seq[Tag]](Seq.empty)
      ~ (__ \ "isArchived"  ).readWithDefault[Boolean](false)
      ~ (__ \ "isDeprecated").readWithDefault[Boolean](false)
      ~ (__ \ "url"         ).read[String]
      ~ Reads.pure(false)
      ~ (__ \ "teamNames"   ).read[Seq[String]]
      ) (apply _)
    }

    val readsDeleted: Reads[Repo] = {
      implicit val readServiceType = ServiceType.reads
      implicit val readTag         = Tag.reads
      ( (__ \ "name"        ).read[String]
      ~ (__ \ "serviceType" ).readNullable[ServiceType]
      ~ (__ \ "tags"        ).readWithDefault[Seq[Tag]](Seq.empty)
      ~ Reads.pure(false)
      ~ Reads.pure(false)
      ~ Reads.pure("")
      ~ Reads.pure(true)
      ~ (__ \ "teamNames"   ).read[Seq[String]]
      ) (apply _)
    }
  }

  sealed trait JobType { def asString: String }

  object JobType {
    case object Job         extends JobType { override val asString = "job"         }
    case object Pipeline    extends JobType { override val asString = "pipeline"    }
    case object PullRequest extends JobType { override val asString = "pull-request"}

    private val logger = Logger(this.getClass)

    val values: List[JobType] =
      List(Job, Pipeline, PullRequest)

    def parse(s: String): JobType =
      values
        .find(_.asString.equalsIgnoreCase(s)).getOrElse {
        logger.info(s"Unable to find job type: $s, defaulted to: job")
        Job
      }

    implicit val reads: Reads[JobType] =
      Reads.of[String].map(parse)
  }

  case class JenkinsJob(
    repoName   : String,
    jenkinsUrl : String,
    jobType    : JobType
  )

  object JenkinsJob {
    implicit val reads: Reads[JenkinsJob] =
      ( (__ \ "repoName"  ).read[String]
      ~ (__ \ "jenkinsURL").read[String]
      ~ (__ \ "jobType"   ).read[JobType]
      )(JenkinsJob.apply _)
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

  private def findRepositories(
    serviceName: Option[ServiceName] = None
  , team: Option[TeamName] = None
  , serviceType: Option[ServiceType] = None
  , repoType: Option[RepoType] = None
  )(implicit hc: HeaderCarrier): Future[Seq[Repo]] = {
    implicit val readRepo: Reads[Repo] = Repo.readsActive

    httpClientV2
      .get(url"$url/api/v2/repositories?repoType=${repoType.map(rt => rt.asString)}&name=${serviceName.map(sn => s"\"${sn.asString}\"")}&team=${team.map(_.asString)}&serviceType=${serviceType.map(_.asString)}")
      .execute[Seq[Repo]]
  }

  def findServiceRepos(
    serviceName: Option[ServiceName] = None
  , team       : Option[TeamName]    = None
  , serviceType: Option[ServiceType] = None
  )(implicit hc: HeaderCarrier): Future[Seq[Repo]] = {
    findRepositories(serviceName = serviceName, team = team, serviceType = serviceType, repoType = Some(Service))
  }

  def getAllRepositories()(implicit hc: HeaderCarrier): Future[Seq[Repo]] = {
    findRepositories()
  }

  def findDeletedServiceRepos(
    serviceName: Option[ServiceName] = None
  , team       : Option[TeamName]    = None
  , serviceType: Option[ServiceType] = None
  )(implicit hc: HeaderCarrier): Future[Seq[Repo]] = {
    implicit val readRepo: Reads[Repo] = Repo.readsDeleted

    httpClientV2
      .get(url"$url/api/deleted-repositories?repoType=service&name=${serviceName.map(sn => s"\"${sn.asString}\"")}&team=${team.map(_.asString)}&serviceType=${serviceType.map(_.asString)}")
      .execute[Seq[Repo]]
  }

  def findJenkinsJobs(repoName: String)(implicit hc: HeaderCarrier): Future[Seq[JenkinsJob]] = {
    implicit val readJobs: Reads[Seq[JenkinsJob]] =
      Reads.at(__ \ "jobs")(Reads.seq(JenkinsJob.reads))

    httpClientV2
      .get(url"$url/api/v2/repositories/$repoName/jenkins-jobs")
      .execute[Seq[JenkinsJob]]
  }
}
