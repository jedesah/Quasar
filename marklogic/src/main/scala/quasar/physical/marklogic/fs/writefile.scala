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

package quasar.physical.marklogic.fs

import quasar.Predef._
import quasar.fp.free.lift
import quasar.fs._, FileSystemError._
import quasar.effect.{KeyValueStore, MonotonicSeq}
import quasar.physical.marklogic.uuid.GenUUID
import quasar.physical.marklogic.xcc.SessionIO

import scalaz._, Scalaz._

object writefile {

  def interpret[S[_]](
    implicit
    S0:      SessionIO :<: S,
    S1:      GenUUID :<: S,
    cursors: KeyValueStore.Ops[WriteFile.WriteHandle, Unit, S],
    seq:     MonotonicSeq.Ops[S]
  ): WriteFile ~> Free[S, ?] = new (WriteFile ~> Free[S, ?]) {
    def apply[A](fa: WriteFile[A]): Free[S, A] = fa match {
      case WriteFile.Open(file) =>
        for {
          id <- seq.next
          writeHandle = WriteFile.WriteHandle(file, id)
          _  <- cursors.put(writeHandle, ())
          r  <- lift(ops.exists(file).ifM(
                  ().right[PathError].point[SessionIO],
                  ops.createFile(file))
                ).into[S]
        } yield r.leftMap(pathErr(_)).as(writeHandle)

      case WriteFile.Write(h, data) =>
        cursors.get(h).isDefined.ifM(
          ops.appendToFile[S](h.file, data),
          Vector(unknownWriteHandle(h)).pure[Free[S, ?]])

      case WriteFile.Close(h) =>
        cursors.delete(h)
    }
  }
}
