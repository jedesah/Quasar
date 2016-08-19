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

package quasar.physical.marklogic.qscript

import quasar.Predef._
import quasar.Planner.PlannerError
import quasar.physical.marklogic.xquery.XQuery
import quasar.qscript._

import matryoshka._
import pathy.Path._
import scalaz._, Scalaz._

private[qscript] final class ReadPlanner extends MarkLogicPlanner[Const[Read, ?]] {
  val plan: AlgebraM[PlannerError \/ ?, Const[Read, ?], XQuery] = {
    case Const(Read(absFile)) =>
      val asDir = fileParent(absFile) </> dir(fileName(absFile).value)
      val dirRepr = posixCodec.printPath(asDir)
      s"""cts:directory-query("$dirRepr", "1")""".right
  }
}
