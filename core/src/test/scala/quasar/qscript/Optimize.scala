/*
 * Copyright 2014–2016 SlamData Inc.
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

package quasar.qscript

import quasar.{LogicalPlan => LP, _}
import quasar.qscript.MapFuncs._
import quasar.fp._
import quasar.fs._

import matryoshka._
import org.specs2.scalaz._
import scalaz._, Scalaz._

class QScriptOptimizeSpec extends CompilerHelpers with QScriptHelpers with ScalazMatchers {
  // TODO instead of calling `.toOption` on the `\/`
  // write an `Equal[PlannerError]` and test for specific errors too
  "optimizer" should {
    "elide a no-op map in a constant boolean" in {
       val query = LP.Constant(Data.Bool(true))
       val run = liftFG(QueryFile.optimize.elideNopMap[QS])

       QueryFile.optimizeEval(run)(query).toOption must
       equal(
         QC.inj(Map(RootR, BoolLit(true))).embed.some)
    }

    "optimize a basic read" in {
      import QueryFile.optimize._

      val run =
        (quasar.fp.free.injectedNT[QS](simplifyProjections).apply(_: QS[Fix[QS]])) ⋙
          liftFG(coalesceMapShift[QS, QS](optionIdF[QS])) ⋙
          Normalizable[QS].normalize ⋙
          liftFG(simplifySP[QS, QS](optionIdF[QS])) ⋙
          liftFG(compactLeftShift[QS, QS])

      val query = lpRead("/foo")

      QueryFile.optimizeEval(run)(query).toOption must
      equal(
        SP.inj(LeftShift(
          RootR,
          ProjectFieldR(HoleF, StrLit("foo")),
          Free.point(RightSide))).embed.some)
    }
  }
}
