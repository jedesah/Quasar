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
import quasar.{Data, DataCodec}
import quasar.fs._
import quasar.fp.free.lift
import quasar.physical.marklogic.uuid._
import quasar.physical.marklogic.xcc._
import quasar.physical.marklogic.xquery._
import quasar.physical.marklogic.xquery.syntax._

import scala.math.{ceil, log}

import com.marklogic.xcc._
import com.marklogic.xcc.exceptions.XQueryException
import com.marklogic.xcc.types.XSBoolean
import pathy.Path._
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process

// TODO: Lots of error handling/existence checking
// TODO: Add support for operations on uri prefixes? i.e. paths that don't map
//       to an actual MarkLogic directory, but look like a directory path
object ops {
  import expr.{if_, for_, select, func}

  def appendToFile[S[_]](
    dstFile: AFile,
    data: Vector[Data]
  )(implicit
    S0: SessionIO :<: S,
    S1: GenUUID :<: S
  ): Free[S, Vector[FileSystemError]] = {
    val dstDir = asDir(dstFile)

    val createOptions = {
      val copts = new ContentCreateOptions()
      copts.setFormatJson()
      copts
    }

    val seqFmt = {
      val width = ceil(log(data.size.toDouble) / log(16)).toInt
      if (width === 0) "" else s"%0${width}x"
    }

    val jsonData: Vector[FileSystemError \/ String] =
      data map { d =>
        DataCodec.render(d)(DataCodec.Precise)
          .leftMap(e => FileSystemError.writeFailed(d, e.message))
      }

    def mkContent(cid: String, seqNum: Int, str: String): Content = {
      val fname = cid + seqFmt.format(seqNum)
      val uri = pathUri(dstDir </> file(fname))
      ContentFactory.newContent(uri, str, createOptions)
    }

    for {
      cid  <- chunkId
      (errs, contents) = jsonData.separate
      cs   =  contents.zipWithIndex map { case (json, i) => mkContent(cid, i, json) }
      exs  <- lift(SessionIO.insertContentCollectErrors(cs)).into[S]
    } yield if (exs.isEmpty) errs else (FileSystemError.partialWrite(errs.length) +: errs)
  }

  def createFile(file: AFile): SessionIO[PathError \/ Unit] = {
    val uri = pathUri(asDir(file))

    // TODO: If dir exists and is empty, then shouldn't error
    handleXcc(SessionIO.executeQuery_(xdmp.directoryCreate(uri.xs)) as ().right[PathError]) {
      case qex: XQueryException if qex.getCode === "XDMP-DIREXISTS" => PathError.pathExists(file).left
    }
  }

  def deleteFile(file: AFile): SessionIO[Unit] = {
    val uri = pathUri(asDir(file))

    SessionIO.executeQuery_(mkSeq_(
      fn.map(func ("$d") { xdmp.documentDelete(fn.baseUri("$d")) }, xdmp.directory(uri.xs, "1".xs)),
      if_ (fn.not(fn.exists(xdmp.directory(uri.xs, "infinity".xs))))
        .then_ { xdmp.directoryDelete(uri.xs) }
        .else_ { expr.emptySeq }
    )).void
  }

  // TODO: Does this fail when the directory isn't empty?
  def deleteDir(dir: ADir): SessionIO[Unit] =
    SessionIO.executeQuery_(xdmp.directoryDelete(pathUri(dir).xs)).void

  // TODO: Can this be implemented with fn:exists(fn:doc($uri))?
  // TODO: How do we separate the file/dir namespace, should this return false if given a file and no docs in dir?
  def exists(path: APath): SessionIO[Boolean] = {
    val uri = pathUri(refineType(path).map(asDir).merge)

    val xqy = fn.exists(fn.filter(func("$uri") { fn.startsWith("$uri", uri.xs) }, cts.uris))

    SessionIO.resultsOf_(xqy) flatMap { items => SessionIO.liftT {
      Task.delay(items(0).asInstanceOf[XSBoolean].asPrimitiveBoolean)
    }}
  }

  def moveFile(src: AFile, dst: AFile): SessionIO[Unit] = {
    def moveXqy = {
      val srcUri = pathUri(asDir(src))
      val dstUri = pathUri(asDir(dst))

      mkSeq_(
        for_("$d" -> xdmp.directory(dstUri.xs, "1".xs))
          .return_(xdmp.documentDelete(xdmp.nodeUri("$d"))),
        for_("$d" -> xdmp.directory(srcUri.xs, "1".xs))
          .let_(
            "$oldName" -> xdmp.nodeUri("$d"),
            "$newName" -> fn.concat(dstUri.xs, select(fn.tokenize("$oldName", "/".xs), fn.last)))
          .return_(mkSeq_(xdmp.documentInsert("$newName", fn.doc("$oldName")), xdmp.documentDelete("$oldName"))))
    }

    if (src === dst) ().point[SessionIO] else SessionIO.executeQuery_(moveXqy).void
  }

  def readFile(file: AFile): Process[ContentSourceIO, Data] = {
    val uri = pathUri(asDir(file))

    val xqy = cts.search(
      fn.doc(),
      cts.directoryQuery(uri.xs),
      IList(cts.indexOrder(cts.uriReference, "ascending".xs)))

    ContentSourceIO.resultStream(SessionIO.evaluateQuery_(xqy))
      .map(xdmitem.toData)
  }

  def subDirs(dir: ADir): SessionIO[Set[RDir]] = {
    val uri = pathUri(dir)

    val xqy =
      for_("$d" -> xdmp.directory(uri.xs, "1".xs))
        .where_(fn.exists("$d/property::directory"))
        .return_(fn.baseUri("$d"))

    SessionIO.resultsOf_(xqy) map (_ foldMap (item =>
      posixCodec.parseAbsDir(item.asString)
        .flatMap(adir => sandboxAbs(adir).relativeTo(dir))
        .toSet))
  }

  ////

  private def asDir(file: AFile): ADir =
    fileParent(file) </> dir(fileName(file).value)

  private def chunkId[S[_]](implicit S: GenUUID :<: S): Free[S, String] =
    GenUUID.Ops[S].asks(id => toSequentialString(id) getOrElse toOpaqueString(id))

  private def pathUri(path: APath): String =
    posixCodec.printPath(path)
}
