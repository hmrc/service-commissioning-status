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

import play.api.mvc.QueryStringBindable

object Binders {

  implicit def serviceNameBindable(implicit strBinder: QueryStringBindable[String]): QueryStringBindable[ServiceName] =
    strBinder.transform(ServiceName.apply, _.asString)

  implicit def serviceTypeBindable(implicit strBinder: QueryStringBindable[String]): QueryStringBindable[ServiceType] =
    new QueryStringBindable[ServiceType] {
      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, ServiceType]] =
        strBinder.bind(key, params).map(_.flatMap(s => ServiceType.parse(s)))
      override def unbind(key: String, value: ServiceType): String =
        strBinder.unbind(key, value.asString)
    }

  implicit def teamNameBindable(implicit strBinder: QueryStringBindable[String]): QueryStringBindable[TeamName] =
    strBinder.transform(TeamName.apply, _.asString)
}
