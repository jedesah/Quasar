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
import quasar.Data
import quasar.effect.{KeyValueStore, MonotonicSeq}
import quasar.fp._
import quasar.fp.free._
import quasar.fs._
import quasar.fs.impl.ReadStream
import quasar.fs.mount.{ConnectionUri, FileSystemDef}, FileSystemDef.DefErrT

import java.net.URI

import com.marklogic.xcc._
import eu.timepit.refined.auto._
import scalaz._, Scalaz._
import scalaz.concurrent.Task

package object fs {
  import ReadFile.ReadHandle, WriteFile.WriteHandle, QueryFile.ResultHandle
  import xcc.{ContentSourceIO, ResultCursor, SessionIO}
  import uuid.GenUUID

  type MLReadHandles[A] = KeyValueStore[ReadHandle, ReadStream[ContentSourceIO], A]
  type MLWriteHandles[A] = KeyValueStore[WriteHandle, Unit, A]
  type MLResultHandles[A] = KeyValueStore[ResultHandle, ResultCursor, A]

  type MarkLogicFs[A]  = Coproduct[Task, MarkLogicFs0, A]
  type MarkLogicFs0[A] = Coproduct[SessionIO, MarkLogicFs1, A]
  type MarkLogicFs1[A] = Coproduct[ContentSourceIO, MarkLogicFs2, A]
  type MarkLogicFs2[A] = Coproduct[GenUUID, MarkLogicFs3, A]
  type MarkLogicFs3[A] = Coproduct[MonotonicSeq, MarkLogicFs4, A]
  type MarkLogicFs4[A] = Coproduct[MLReadHandles, MarkLogicFs5, A]
  type MarkLogicFs5[A] = Coproduct[MLWriteHandles, MLResultHandles, A]

  val FsType = FileSystemType("marklogic")

  def definition[S[_]](
    implicit
    S0: Task :<: S,
    S1: PhysErr :<: S
  ): FileSystemDef[Free[S, ?]] =
    FileSystemDef.fromPF {
      case (FsType, uri) =>
        lift(runMarkLogicFs(uri).map { run =>
          FileSystemDef.DefinitionResult[Free[S, ?]](
            mapSNT(injectNT[Task, S] compose run) compose interpretFileSystem(
              queryfile.interpret[MarkLogicFs](10000L),
              readfile.interpret[MarkLogicFs](10000L),
              writefile.interpret[MarkLogicFs],
              managefile.interpret[MarkLogicFs]),
            ().point[Free[S, ?]])
        }).into[S].liftM[DefErrT]
    }

  def runMarkLogicFs(connectionUri: ConnectionUri): Task[MarkLogicFs ~> Task] = {
    val uri = new URI(connectionUri.value)

    (
      KeyValueStore.impl.empty[WriteHandle, Unit]                       |@|
      KeyValueStore.impl.empty[ReadHandle, ReadStream[ContentSourceIO]] |@|
      KeyValueStore.impl.empty[ResultHandle, ResultCursor]              |@|
      MonotonicSeq.fromZero                                             |@|
      GenUUID.type1                                                     |@|
      // TODO: Catch any XccConfigExceptions thrown here and returns as config errors
      Task.delay(ContentSourceFactory.newContentSource(uri))
    ) { (whandles, rhandles, qhandles, seq, genUUID, csource) =>
      val runCSIO = ContentSourceIO.runNT(csource)
      val runSIO  = runCSIO compose ContentSourceIO.runSessionIO

      reflNT[Task] :+: runSIO :+: runCSIO :+: genUUID :+: seq :+: rhandles :+: whandles :+: qhandles
    }
  }

  implicit val resultCursorDataCursor: DataCursor[Task, ResultCursor] =
    new DataCursor[Task, ResultCursor] {
      def close(rc: ResultCursor) =
        rc.close.void

      def nextChunk(rc: ResultCursor) =
        rc.nextChunk.map(_.foldLeft(Vector[Data]())((ds, x) =>
          ds :+ xdmitem.toData(x)))
    }
}
