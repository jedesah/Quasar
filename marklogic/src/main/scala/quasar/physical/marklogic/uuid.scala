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

package quasar.physical.marklogic

import quasar.Predef._
import quasar.effect.Read

import java.util.UUID

import com.fasterxml.uuid._
import scalaz.{:<:, ~>}
import scalaz.std.anyVal._
import scalaz.syntax.equal._
import scalaz.concurrent.Task

object uuid {
  type GenUUID[A] = Read[UUID, A]

  object GenUUID {
    def Ops[S[_]](implicit S: GenUUID :<: S) =
      Read.Ops[UUID, S]

    val type1: Task[GenUUID ~> Task] =
      Task.delay(
        fromNoArg(Option(EthernetAddress.fromInterface).fold(
          Generators.timeBasedGenerator)(
          Generators.timeBasedGenerator)))

    private def fromNoArg(noArgGen: NoArgGenerator): GenUUID ~> Task =
      new (GenUUID ~> Task) {
        def apply[A](ga: GenUUID[A]) = ga match {
          case Read.Ask(f) => Task.delay(f(noArgGen.generate))
        }
      }
  }

  /** Returns an opaque string from the given UUID */
  def toOpaqueString(uuid: UUID): String =
    uuid.toString.replace("-", "")

  /** Returns an opaque string from the given UUID that is sequential w.r.t.
    * lexigraphical ordering for UUID Type-1 variants. That is, if a: UUID and
    * b: UUID and `b` was generated after `a` then
    *
    *   `toSequentialString(a) < toSequentialString(b) == true`
    *
    * returns None if the given UUID is not Type-1.
    */
  def toSequentialString(uuid: UUID): Option[String] =
    if (uuid.version === 1) {
      val parts = uuid.toString.split("-")
      Some(s"${parts(2)}${parts(1)}${parts(0)}${parts(3)}${parts(4)}")
    } else None
}
