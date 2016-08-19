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

import quasar.Predef._
import quasar.{LogicalPlan => LP}
import quasar.fs._
import quasar.qscript.MapFuncs._

import scala.Predef.implicitly

import matryoshka._
import pathy.Path._
import scalaz._

trait QScriptHelpers {
  // TODO: Narrow this to QScriptPure
  type QS[A] = QScriptTotal[Fix, A]
  val DE = implicitly[Const[DeadEnd, ?] :<: QS]
  val QC = implicitly[QScriptCore[Fix, ?] :<: QS]
  val SP = implicitly[SourcedPathable[Fix, ?] :<: QS]
  val TJ = implicitly[ThetaJoin[Fix, ?] :<: QS]

  def RootR: Fix[QS] = CorecursiveOps[Fix, QS](DE.inj(Const[DeadEnd, Fix[QS]](Root))).embed

  def ProjectFieldR[A](src: Free[MapFunc[Fix, ?], A], field: Free[MapFunc[Fix, ?], A]): Free[MapFunc[Fix, ?], A] =
    Free.roll(ProjectField(src, field))

  def lpRead(path: String): Fix[LP] =
    LP.Read(sandboxAbs(posixCodec.parseAbsFile(path).get))
}
