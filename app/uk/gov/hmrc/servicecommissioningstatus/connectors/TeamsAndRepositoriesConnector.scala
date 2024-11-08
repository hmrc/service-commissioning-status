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
import play.api.libs.json.{Reads, __}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpReads, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.servicecommissioningstatus.{FromString, Parser, ServiceName, ServiceType, TeamName}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.servicecommissioningstatus.FromStringEnum._

object TeamsAndRepositoriesConnector:
  import play.api.libs.functional.syntax.*
  import play.api.libs.json.*

  enum Tag(val asString: String) extends FromString derives Reads:
    case AdminFrontend    extends Tag("admin"             )
    case Api              extends Tag("api"               )
    case BuiltOffPlatform extends Tag("built-off-platform")
    case Maven            extends Tag("maven"             )
    case Stub             extends Tag("stub"              )
    
  object Tag:
    given Parser[Tag] = Parser.parser(Tag.values)

  case class Repo(
    name        : String
  , serviceType : Option[ServiceType] = None
  , tags        : Seq[Tag] = Seq.empty
  , isArchived  : Boolean
  , isDeprecated: Boolean
  , githubUrl   : String
  , isDeleted   : Boolean
  )

  object Repo:
    val readsActive: Reads[Repo] =
      ( (__ \ "name"        ).read[String]
      ~ (__ \ "serviceType" ).readNullable[ServiceType]
      ~ (__ \ "tags"        ).readWithDefault[Seq[Tag]](Seq.empty)
      ~ (__ \ "isArchived"  ).readWithDefault[Boolean](false)
      ~ (__ \ "isDeprecated").readWithDefault[Boolean](false)
      ~ (__ \ "url"         ).read[String]
      ~ Reads.pure(false)
      ) (apply _)

    val readsDeleted: Reads[Repo] =
      ( (__ \ "name"        ).read[String]
      ~ (__ \ "serviceType" ).readNullable[ServiceType]
      ~ (__ \ "tags"        ).readWithDefault[Seq[Tag]](Seq.empty)
      ~ Reads.pure(false)
      ~ Reads.pure(false)
      ~ Reads.pure("")
      ~ Reads.pure(true)
      ) (apply _)

  enum JobType(val asString: String):
    case Job         extends JobType("job")
    case Pipeline    extends JobType("pipeline")
    case PullRequest extends JobType("pull-request")

  object JobType extends Logging:
    def parse(s: String): JobType =
      values
        .find(_.asString.equalsIgnoreCase(s)).getOrElse {
        logger.info(s"Unable to find job type: $s, defaulted to: job")
        Job
      }

    given Reads[JobType] =
      Reads.of[String].map(parse)

  case class JenkinsJob(
    repoName   : String,
    jenkinsUrl : String,
    jobName    : String,
    jobType    : JobType
  )

  object JenkinsJob:
    given reads: Reads[JenkinsJob] =
      ( (__ \ "repoName"  ).read[String]
      ~ (__ \ "jenkinsURL").read[String]
      ~ (__ \ "jobName"   ).read[String]
      ~ (__ \ "jobType"   ).read[JobType]
      )(JenkinsJob.apply _)

@Singleton
class TeamsAndRepositoriesConnector @Inject()(
  httpClientV2  : HttpClientV2,
  servicesConfig: ServicesConfig
)(using ExecutionContext):
  import HttpReads.Implicits.*
  import TeamsAndRepositoriesConnector.*

  private val url = servicesConfig.baseUrl("teams-and-repositories")

  def findServiceRepos(
    serviceName: Option[ServiceName] = None
  , team       : Option[TeamName]    = None
  , serviceType: Option[ServiceType] = None
  )(using HeaderCarrier): Future[Seq[Repo]] =
    given Reads[Repo] = Repo.readsActive

    httpClientV2
      .get(url"$url/api/v2/repositories?repoType=service&name=${serviceName.map(sn => s"\"${sn.asString}\"")}&team=${team.map(_.asString)}&serviceType=${serviceType.map(_.asString)}")
      .execute[Seq[Repo]]

  def findDeletedServiceRepos(
    serviceName: Option[ServiceName] = None
  , team       : Option[TeamName]    = None
  , serviceType: Option[ServiceType] = None
  )(using HeaderCarrier): Future[Seq[Repo]] =
    given Reads[Repo] = Repo.readsDeleted

    httpClientV2
      .get(url"$url/api/deleted-repositories?repoType=service&name=${serviceName.map(sn => s"\"${sn.asString}\"")}&team=${team.map(_.asString)}&serviceType=${serviceType.map(_.asString)}")
      .execute[Seq[Repo]]

  def findJenkinsJobs(repoName: String)(using HeaderCarrier): Future[Seq[JenkinsJob]] =
    given Reads[Seq[JenkinsJob]] =
      Reads.at(__ \ "jobs")(Reads.seq(JenkinsJob.reads))

    httpClientV2
      .get(url"$url/api/v2/repositories/$repoName/jenkins-jobs")
      .execute[Seq[JenkinsJob]]

  def findAssociatedTestRepos(repoName: String)(using HeaderCarrier): Future[Seq[String]] =
    httpClientV2
      .get(url"$url/api/v2/repositories/$repoName/test-repositories")
      .execute[Seq[String]]
