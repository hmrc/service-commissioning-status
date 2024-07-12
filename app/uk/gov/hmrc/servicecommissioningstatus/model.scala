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

object ServiceName:
  val format: Format[ServiceName] =
    Format.of[String].inmap(ServiceName.apply, _.asString)

enum ServiceType(val asString: String):
  case Frontend extends ServiceType("frontend")
  case Backend  extends ServiceType("backend")

object ServiceType:
  def parse(s: String): Either[String, ServiceType] =
    values
      .find(_.asString.equalsIgnoreCase(s))
      .toRight(s"Invalid value: \"$s\" - should be one of: ${values.map(_.asString).mkString(", ")}")

  val reads: Reads[ServiceType] =
    _.validate[String]
      .flatMap(parse(_).fold(JsError(_), JsSuccess(_)))

enum Environment(val asString: String, val displayString: String):
  case Integration  extends Environment(asString = "integration" , displayString = "Integration"   )
  case Development  extends Environment(asString = "development" , displayString = "Development"   )
  case QA           extends Environment(asString = "qa"          , displayString = "QA"            )
  case Staging      extends Environment(asString = "staging"     , displayString = "Staging"       )
  case ExternalTest extends Environment(asString = "externaltest", displayString = "External Test" )
  case Production   extends Environment(asString = "production"  , displayString = "Production"    )

object Environment:
  def parse(s: String): Either[String, Environment] =
    values
      .find(_.asString.equalsIgnoreCase(s))
      .toRight(s"Invalid value: \"$s\" - should be one of: ${values.map(_.asString).mkString(", ")}")

  val format: Format[Environment] = new Format[Environment]:
    override def writes(o: Environment): JsValue = JsString(o.asString)
    override def reads(json: JsValue): JsResult[Environment] =
      json.validate[String].flatMap(s => Environment.parse(s).map(e => JsSuccess(e)).getOrElse(JsError("invalid environment")))

enum Result(val isPresent: Boolean):
  case Missing(addLink: String) extends Result(false)
  case Present(evidenceLink: String) extends Result(true)

enum Check:
  case SimpleCheck(
    title: String,
    result: Result,
    helpText: String,
    linkToDocs: Option[String]
  ) extends Check

  case EnvCheck(
    title: String,
    results: Map[Environment, Result],
    helpText: String,
    linkToDocs: Option[String]
  ) extends Check

object Check:
  val format: Format[Check] =
    given Format[Result] = Format(
      (json: JsValue) =>
        ((json \ "evidence").asOpt[String], (json \ "add").asOpt[String]) match
          case (Some(str), _) => JsSuccess(Result.Present(str))
          case (_, Some(str)) => JsSuccess(Result.Missing(str))
          case _ => JsError("Could not find either field 'evidence' or 'add'")
    , {
        case Result.Missing(v) => Json.obj("add" -> v)
        case Result.Present(v) => Json.obj("evidence" -> v)
      }
    )

    given Format[SimpleCheck] =
      ( (__ \ "title"      ).format[String]
      ~ (__ \ "simpleCheck").format[Result]
      ~ (__ \ "helpText"   ).format[String]
      ~ (__ \ "linkToDocs" ).formatNullable[String]
      )(SimpleCheck.apply, s => Tuple.fromProductTyped(s))

    given Format[Map[Environment, Result]] =
      Format(
        Reads
          .of[Map[String, Result]]
          .map(_.map { case (k, v) => (Environment.parse(k).getOrElse(sys.error(s"Invalid Environment: $k")), v) })
      , Writes
          .apply(xs => Json.toJson(xs.map { case (k, v) => k.asString -> v }))
      )

    given Format[EnvCheck] =
      ( (__ \ "title"           ).format[String]
      ~ (__ \ "environmentCheck").format[Map[Environment, Result]]
      ~ (__ \ "helpText"        ).format[String]
      ~ (__ \ "linkToDocs"      ).formatNullable[String]
      )(EnvCheck.apply, e => Tuple.fromProductTyped(e))

    Format(
      (json: JsValue) =>
        json
          .validate[SimpleCheck]
          .orElse(json.validate[EnvCheck])
    , {
        case a: SimpleCheck => Json.toJson(a)
        case b: EnvCheck    => Json.toJson(b)
      }
    )

case class Warning(
  title: String
, message: String
)

object Warning:
  val format: OFormat[Warning] =
    ( (__ \ "title"  ).format[String]
    ~ (__ \ "message").format[String]
    )(Warning.apply, w => Tuple.fromProductTyped(w))
