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

package uk.gov.hmrc.servicecommissioningstatus

import play.api.libs.functional.syntax._
import play.api.libs.json._

case class TeamName(asString: String) extends AnyVal

case class ServiceName(asString: String) extends AnyVal

object ServiceName {
  val format: Format[ServiceName] =
    Format.of[String].inmap(ServiceName.apply, unlift(ServiceName.unapply))
}


sealed trait ServiceType extends WithAsString


object ServiceType extends Enum[ServiceType] {
  case object Frontend extends ServiceType { val asString = "frontend" }
  case object Backend  extends ServiceType { val asString = "backend"  }

  override val values: List[ServiceType] = List(Frontend, Backend)
}

sealed trait Environment extends WithAsString
object Environment extends Enum[Environment] {
  case object Development  extends Environment { val asString = "development" }
  case object Integration  extends Environment { val asString = "integration" }
  case object QA           extends Environment { val asString = "qa"          }
  case object Staging      extends Environment { val asString = "staging"     }
  case object ExternalTest extends Environment { val asString = "externaltest"}
  case object Production   extends Environment { val asString = "production"  }

  override val values: List[Environment] =
    List(Development, Integration, QA, Staging, ExternalTest, Production)
}


sealed trait Check {
  val title     : String
  val helpText  : String
  val linkToDocs: Option[String]
}

object Check {
  case class Missing(addLink: String)
  case class Present(evidenceLink: String)

  type Result = Either[Missing, Present] // TODO make ADT?

  sealed case class SimpleCheck(
    title     : String
  , result    : Result
  , helpText  : String
  , linkToDocs: Option[String]
  ) extends Check

  sealed case class EnvCheck(
    title     : String
  , results   : Map[Environment, Result]
  , helpText  : String
  , linkToDocs: Option[String]
  ) extends Check

  val format: Format[Check] = {
    implicit val formatResult: Format[Result] = Format(
      (json: JsValue) =>
        ((json \ "evidence").asOpt[String], (json \ "add").asOpt[String]) match {
          case (Some(str), _) => JsSuccess(Right(Present(str)): Result)
          case (_, Some(str)) => JsSuccess(Left(Missing(str)): Result)
          case _ => JsError("Could not find either field 'evidence' or 'add'")
      }
    , {
        case Left(Missing(v)) => Json.obj("add" -> v)
        case Right(Present(v)) => Json.obj("evidence" -> v)
      }
    )

    implicit val formatSimpleCheck: Format[SimpleCheck] =
      ( (__ \ "title"      ).format[String]
      ~ (__ \ "simpleCheck").format[Result]
      ~ (__ \ "helpText"   ).format[String]
      ~ (__ \ "linkToDocs" ).formatNullable[String]
      )(SimpleCheck.apply, unlift(SimpleCheck.unapply))

    implicit val formatEnvMap: Format[Map[Environment, Result]] = Format(
      Reads
        .of[Map[String, Result]]
        .map(_.map { case (k, v) => (Environment.parse(k).getOrElse(sys.error(s"Invalid Environment: $k")), v) })
    , Writes
        .apply { xs => Json.toJson(xs.map { case (k, v) => k.asString -> v }) }
    )

    implicit val formatEnvCheck: Format[EnvCheck] =
      ( (__ \ "title"           ).format[String]
      ~ (__ \ "environmentCheck").format[Map[Environment, Result]]
      ~ (__ \ "helpText"        ).format[String]
      ~ (__ \ "linkToDocs"      ).formatNullable[String]
      )(EnvCheck.apply, unlift(EnvCheck.unapply))

    Format(
      (json: JsValue) => json
        .validate[SimpleCheck]
        .orElse(json.validate[EnvCheck])
    , {
        case a: SimpleCheck => Json.toJson(a)
        case b: EnvCheck => Json.toJson(b)
      }
    )
  }
}
