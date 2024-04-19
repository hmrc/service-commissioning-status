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

package uk.gov.hmrc.servicecommissioningstatus.persistence

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport
import uk.gov.hmrc.servicecommissioningstatus._

import scala.concurrent.ExecutionContext.Implicits.global

class CacheRepositorySpec
  extends AnyWordSpec
     with Matchers
     with DefaultPlayMongoRepositorySupport[CacheRepository.ServiceCheck] {
  import CacheRepository._

  override protected val repository = new CacheRepository(mongoComponent)

  val serviceCheck1 = ServiceCheck(
    serviceName     = ServiceName("service1")
  , lifecycleStatus = LifecycleStatus.Active
  , checks          = Seq(Check.SimpleCheck(title = "title1", result = Right(Check.Present("link1")), helpText = "help1", linkToDocs = None))
  )
  val serviceCheck2 = ServiceCheck(
    serviceName     = ServiceName("service2")
  , lifecycleStatus = LifecycleStatus.Archived
  , checks          = Seq(Check.SimpleCheck(title = "title2", result = Left(Check.Missing("link2")), helpText = "help2", linkToDocs = None))
  )
  val serviceCheck3 = ServiceCheck(
    serviceName     = ServiceName("service3")
  , lifecycleStatus = LifecycleStatus.DecommissionInProgress
  , checks          = Seq(Check.SimpleCheck(title = "title3", result = Right(Check.Present("link3")), helpText = "help3", linkToDocs = None))
  )

  "CacheRepository.putAll" should {
    "put correctly" in {
      repository.putAll(Seq(serviceCheck1, serviceCheck2)).futureValue
      findAll().futureValue should contain.only(serviceCheck1, serviceCheck2)

      repository.putAll(Seq(serviceCheck2.copy(lifecycleStatus = LifecycleStatus.Deprecated), serviceCheck3)).futureValue
      findAll().futureValue should contain.only(serviceCheck2.copy(lifecycleStatus = LifecycleStatus.Deprecated), serviceCheck3)
    }
  }

  "CacheRepository.findAll" should {
    "find matches" in {
      repository.putAll(Seq(serviceCheck1, serviceCheck2, serviceCheck3)).futureValue

      repository.findAll(serviceNames = Seq(ServiceName("service1")), lifecycleStatus = Seq.empty).futureValue shouldBe Seq(serviceCheck1)
      repository.findAll(serviceNames = Seq.empty, lifecycleStatus = Seq(LifecycleStatus.Archived)).futureValue shouldBe Seq(serviceCheck2)
    }
  }
}
