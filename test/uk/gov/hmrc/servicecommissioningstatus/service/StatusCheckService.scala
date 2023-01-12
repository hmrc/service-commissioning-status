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

package uk.gov.hmrc.servicecommissioningstatus.service

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import uk.gov.hmrc.servicecommissioningstatus.model.{Environment, Check}

class StatusCheckServiceSpec extends AnyWordSpec with Matchers {
  import Check._; import Environment._

  private val missing: Result = Left( Missing(""))
  private val present: Result = Right(Present(""))

  private val allPresent: Map[Environment, Result] =
    Map(Integration -> present, Development -> present, QA -> present, Staging -> present, ExternalTest -> present, Production -> present)

  private val allMissing: Map[Environment, Result] =
    Map(Integration -> missing, Development -> missing, QA -> missing, Staging -> missing, ExternalTest -> missing, Production -> missing)

  "Checks" should {
    "hide Integration when unconfigured" in {
      StatusCheckService.hideUnconfiguredEnvironments(
        checks       =  SimpleCheck(title = "A", result  = present                              ) ::
                        SimpleCheck(title = "B", result  = present                              ) ::
                        EnvCheck   (title = "C", results = allPresent + (Integration -> missing)) ::
                        SimpleCheck(title = "D", result  = present                              ) ::
                        EnvCheck   (title = "E", results = allPresent + (Integration -> missing)) ::
                        EnvCheck   (title = "F", results = allPresent + (Integration -> missing)) ::
                        SimpleCheck(title = "G", result  = present                              ) ::
                        Nil
      , environments = Set(Environment.Integration, Environment.Development)
      ) shouldBe (
        SimpleCheck(title = "A", result  = present                 ) ::
        SimpleCheck(title = "B", result  = present                 ) ::
        EnvCheck   (title = "C", results = allPresent - Integration) ::
        SimpleCheck(title = "D", result  = present                 ) ::
        EnvCheck   (title = "E", results = allPresent - Integration) ::
        EnvCheck   (title = "F", results = allPresent - Integration) ::
        SimpleCheck(title = "G", result  = present                 ) ::
        Nil
      )
    }

    "show Integration when partially configured" in {
      val checks = SimpleCheck(title = "A", result  = present                              ) ::
                   SimpleCheck(title = "B", result  = present                              ) ::
                   EnvCheck   (title = "C", results = allPresent + (Integration -> missing)) ::
                   SimpleCheck(title = "D", result  = present                              ) ::
                   EnvCheck   (title = "E", results = allPresent                           ) :: // Configured here
                   EnvCheck   (title = "F", results = allPresent + (Integration -> missing)) ::
                   SimpleCheck(title = "G", result  = present                              ) ::
                   Nil
      StatusCheckService.hideUnconfiguredEnvironments(
        checks       = checks
      , environments = Set(Environment.Integration, Environment.Development)
      ) shouldBe (checks) // Unchanged
    }

    "hide Integration and Development unconfigured" in {
      StatusCheckService.hideUnconfiguredEnvironments(
        checks       =  SimpleCheck(title = "A", result  = present                                                          ) ::
                        SimpleCheck(title = "B", result  = present                                                          ) ::
                        EnvCheck   (title = "C", results = allPresent ++ Map(Integration -> missing, Development -> missing)) ::
                        SimpleCheck(title = "D", result  = present                                                          ) ::
                        EnvCheck   (title = "E", results = allPresent ++ Map(Integration -> missing, Development -> missing)) ::
                        EnvCheck   (title = "F", results = allPresent ++ Map(Integration -> missing, Development -> missing)) ::
                        SimpleCheck(title = "G", result  = present                                                          ) ::
                        Nil
      , environments = Set(Environment.Integration, Environment.Development)
      ) shouldBe (
        SimpleCheck(title = "A", result  = present                                              ) ::
        SimpleCheck(title = "B", result  = present                                              ) ::
        EnvCheck   (title = "C", results = allPresent.removedAll(List(Integration, Development))) ::
        SimpleCheck(title = "D", result  = present                                              ) ::
        EnvCheck   (title = "E", results = allPresent.removedAll(List(Integration, Development))) ::
        EnvCheck   (title = "F", results = allPresent.removedAll(List(Integration, Development))) ::
        SimpleCheck(title = "G", result  = present                                              ) ::
        Nil
      )
    }

    "show QA, Staging, ExternalTest & Production when unconfigured" in {
      StatusCheckService.hideUnconfiguredEnvironments(
        checks       =  SimpleCheck(title = "A", result  = missing   ) ::
                        SimpleCheck(title = "B", result  = missing   ) ::
                        EnvCheck   (title = "C", results = allMissing) ::
                        SimpleCheck(title = "D", result  = missing   ) ::
                        EnvCheck   (title = "E", results = allMissing) ::
                        EnvCheck   (title = "F", results = allMissing) ::
                        SimpleCheck(title = "G", result  = missing   ) ::
                        Nil
      , environments = Set(Environment.Integration, Environment.Development)
      ) shouldBe (
        SimpleCheck(title = "A", result  = missing                                              ) ::
        SimpleCheck(title = "B", result  = missing                                              ) ::
        EnvCheck   (title = "C", results = allMissing.removedAll(List(Integration, Development))) ::
        SimpleCheck(title = "D", result  = missing                                              ) ::
        EnvCheck   (title = "E", results = allMissing.removedAll(List(Integration, Development))) ::
        EnvCheck   (title = "F", results = allMissing.removedAll(List(Integration, Development))) ::
        SimpleCheck(title = "G", result  = missing                                              ) ::
        Nil
      )
    }
  }
}
