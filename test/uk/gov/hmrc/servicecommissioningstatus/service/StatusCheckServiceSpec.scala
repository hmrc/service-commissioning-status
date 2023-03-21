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
        checks       =  SimpleCheck(
                          title      = "A",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "B",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "C",
                          results    = allPresent + (Integration -> missing),
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "D",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "E",
                          results    = allPresent + (Integration -> missing),
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "F",
                          results    = allPresent + (Integration -> missing),
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "G",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        Nil
      , environments = Set(Environment.Integration, Environment.Development)
      ) shouldBe (
        SimpleCheck(
          title      = "A",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "B",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "C",
          results    = allPresent - Integration,
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "D",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "E",
          results    = allPresent - Integration,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "F",
          results    = allPresent - Integration,
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "G",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        Nil
      )
    }

    "show Integration when partially configured" in {
      val checks = SimpleCheck(
                     title      = "A",
                     result     = present,
                     helpText   = "",
                     linkToDocs = None
                   ) ::
                   SimpleCheck(
                     title      = "B",
                     result     = present,
                     helpText   = "",
                     linkToDocs = None
                   ) ::
                   EnvCheck(
                     title      = "C",
                     results    = allPresent + (Integration -> missing),
                     helpText   = "",
                     linkToDocs = None
                   ) ::
                   SimpleCheck(
                     title      = "D",
                     result     = present,
                     helpText   = "",
                     linkToDocs = None
                   ) ::
                   EnvCheck(
                     title      = "E",
                     results    = allPresent, // Configured here
                     helpText   = "",
                     linkToDocs = None
                   ) ::
                   EnvCheck(
                     title      = "F",
                     results    = allPresent + (Integration -> missing),
                     helpText   = "",
                     linkToDocs = None
                   ) ::
                   SimpleCheck(
                     title      = "G",
                     result     = present,
                     helpText   = "",
                     linkToDocs = None
                   ) ::
                   Nil
      StatusCheckService.hideUnconfiguredEnvironments(
        checks       = checks
      , environments = Set(Environment.Integration, Environment.Development)
      ) shouldBe (checks) // Unchanged
    }

    "hide Integration and Development unconfigured" in {
      StatusCheckService.hideUnconfiguredEnvironments(
        checks       =  SimpleCheck(
                          title      = "A",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "B",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "C",
                          results    = allPresent ++ Map(Integration -> missing, Development -> missing),
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "D",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "E",
                          results    = allPresent ++ Map(Integration -> missing, Development -> missing),
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "F",
                          results    = allPresent ++ Map(Integration -> missing, Development -> missing),
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "G",
                          result     = present,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        Nil
      , environments = Set(Environment.Integration, Environment.Development)
      ) shouldBe (
        SimpleCheck(
          title      = "A",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "B",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "C",
          results    = allPresent.removedAll(List(Integration, Development)),
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "D",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "E",
          results    = allPresent.removedAll(List(Integration, Development)),
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "F",
          results    = allPresent.removedAll(List(Integration, Development)),
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "G",
          result     = present,
          helpText   = "",
          linkToDocs = None
        ) ::
        Nil
      )
    }

    "show QA, Staging, ExternalTest & Production when unconfigured" in {
      StatusCheckService.hideUnconfiguredEnvironments(
        checks       =  SimpleCheck(
                          title      = "A",
                          result     = missing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "B",
                          result     = missing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "C",
                          results    = allMissing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "D",
                          result     = missing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "E",
                          results    = allMissing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        EnvCheck(
                          title      = "F",
                          results    = allMissing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        SimpleCheck(
                          title      = "G",
                          result     = missing,
                          helpText   = "",
                          linkToDocs = None
                        ) ::
                        Nil
      , environments = Set(Environment.Integration, Environment.Development)
      ) shouldBe (
        SimpleCheck(
          title      = "A",
          result     = missing,
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "B",
          result     = missing,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "C",
          results    = allMissing.removedAll(List(Integration, Development)),
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "D",
          result     = missing,
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "E",
          results    = allMissing.removedAll(List(Integration, Development)),
          helpText   = "",
          linkToDocs = None
        ) ::
        EnvCheck(
          title      = "F",
          results    = allMissing.removedAll(List(Integration, Development)),
          helpText   = "",
          linkToDocs = None
        ) ::
        SimpleCheck(
          title      = "G",
          result     = missing,
          helpText   = "",
          linkToDocs = None
        ) ::
        Nil
      )
    }
  }
}
