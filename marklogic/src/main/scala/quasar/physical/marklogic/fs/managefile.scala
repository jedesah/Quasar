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
import quasar.fs._
import quasar.physical.marklogic.xcc.SessionIO

import scala.util.Random

import pathy.Path._
import scalaz._, Scalaz._
import scalaz.concurrent.Task

object managefile {
  import ManageFile._

  def interpret[S[_]](implicit S: SessionIO :<: S): ManageFile ~> Free[S, ?] =
    new (ManageFile ~> Free[S, ?]) {
      def apply[A](fs: ManageFile[A]) = fs match {
        case Move(scenario, semantics) => move(scenario, semantics)
        case Delete(path)              => delete(path)
        case TempFile(path)            => tempFile(path)
      }

      def move(scenario: MoveScenario, semantics: MoveSemantics): Free[S, FileSystemError \/ Unit] =
        lift(scenario match {
          case MoveScenario.FileToFile(src, dst) =>
            ops.moveFile(src, dst).map(_.right[FileSystemError])

          case MoveScenario.DirToDir(src, dst) => ???
        }).into[S]

      def delete(path: APath): Free[S, FileSystemError \/ Unit] =
        lift(refineType(path).fold(ops.deleteDir, ops.deleteFile))
          .into[S]
          .map(_.right)

      def tempFile(path: APath): Free[S, FileSystemError \/ AFile] =
        tempName map { fname =>
          refineType(path).fold(
            d => d </> file(fname),
            f => fileParent(f) </> file(fname)
          ).right
        }

      val tempName: Free[S, String] =
        lift(SessionIO.liftT(
          Task.delay("temp-" + Random.alphanumeric.take(10).mkString)
        )).into[S]
    }
}
