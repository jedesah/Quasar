/*
 * Copyright 2014–2018 SlamData Inc.
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

import quasar.contrib.pathy.{ADir, UriPathCodec}
import quasar.physical.marklogic.cts._
import quasar.physical.marklogic.xquery._
import quasar.qscript._

import eu.timepit.refined.auto._
import matryoshka._
import scalaz._, Scalaz._

private[qscript] final class ShiftedReadDirPlanner[F[_]: Applicative: MonadPlanErr, FMT, J]
    extends Planner[F, FMT, Const[ShiftedRead[ADir], ?], J] {

  import MarkLogicPlannerError._

  def plan[Q](implicit Q: Birecursive.Aux[Q, Query[J, ?]]
  ): AlgebraM[F, Const[ShiftedRead[ADir], ?], Search[Q] \/ XQuery] = {
    case Const(ShiftedRead(dir, idStatus)) =>
      val dirUri = UriPathCodec.printPath(dir)

      Uri.getOption(dirUri).cata(uri =>
        Search(
          Q.embed(Query.Directory[J, Q](IList(uri), MatchDepth.Children)),
          idStatus,
          IList()
        ).left[XQuery].point[F],
        MonadPlanErr[F].raiseError(invalidUri(dirUri)))
  }
}
