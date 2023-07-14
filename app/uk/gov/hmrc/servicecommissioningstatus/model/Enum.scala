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

import play.api.libs.json._

trait WithAsString {def asString: String}

trait Enum[T <: WithAsString] {
  val values: List[T]

  def parse(s: String): Either[String, T] =
    values
      .find(_.asString.equalsIgnoreCase(s))
      .toRight(s"Invalid value: \"$s\" - should be one of: ${values.map(_.asString).mkString(", ")}")



  val format: Format[T] = new Format[T] {
    override def writes(o: T): JsValue =  JsString(o.asString)

    override def reads(json: JsValue): JsResult[T] =
      json.validate[String]
        .flatMap(parse(_).fold(JsError(_), JsSuccess(_)))

  }
}
