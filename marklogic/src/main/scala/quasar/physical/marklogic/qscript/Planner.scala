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

import quasar.Planner.PlannerError

import matryoshka._
import scalaz._

trait Planner[QS[_], A] {
  def plan: AlgebraM[PlannerError \/ ?, QS, A]
}

object Planner {
  def apply[QS[_], A](implicit ev: Planner[QS, A]): Planner[QS, A] = ev

  implicit def coproduct[A, F[_], G[_]](implicit F: Planner[F, A], G: Planner[G, A]): Planner[Coproduct[F, G, ?], A] =
    new Planner[Coproduct[F, G, ?], A] {
      def plan: AlgebraM[PlannerError \/ ?, Coproduct[F, G, ?], A] =
        _.run.fold(F.plan, G.plan)
    }
}