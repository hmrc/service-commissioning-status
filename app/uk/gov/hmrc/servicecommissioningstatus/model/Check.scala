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

package uk.gov.hmrc.servicecommissioningstatus.model

sealed trait Check

object Check {
  case class Missing(addLink: String)
  case class Present(evidenceLink: String)

  type Result = Either[Missing, Present]

  sealed case class SimpleCheck(
    title : String
  , result: Result
  ) extends Check

  sealed case class EnvCheck(
    title  : String
  , results: Map[Environment, Result]
  ) extends Check

  import play.api.libs.functional.syntax.{toFunctionalBuilderOps, unlift}
  import play.api.libs.json.{Json, Writes, __}
  val writes: Writes[Check] = new Writes[Check] {

    implicit val writesResult: Writes[Result] = new Writes[Result] {
      def writes(result: Result) = result match {
        case Left(Missing(v))  => Json.obj("add" -> v)
        case Right(Present(v)) => Json.obj("evidence" -> v)
      }
    }

    implicit val writesSimpleCheck: Writes[SimpleCheck] =
      ( (__ \ "title"      ).write[String]
      ~ (__ \ "simpleCheck").write[Result]
      ) (unlift(SimpleCheck.unapply))

    implicit val mapFormat: Writes[Map[Environment, Result]] =
      Writes
        .of[Map[String, Result]]
        .contramap(
          _.map { case (k, v) => (k.asString, v) }
        )

    implicit val writesEnvCheck: Writes[EnvCheck] =
      ( (__ \ "title"           ).write[String]
      ~ (__ \ "environmentCheck").write[Map[Environment, Result]]
      ) (unlift(EnvCheck.unapply))

    def writes(check: Check) = check match {
      case a: SimpleCheck => Json.toJson(a)
      case b: EnvCheck    => Json.toJson(b)
    }
  }
}
