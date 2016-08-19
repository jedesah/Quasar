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

package quasar.physical.marklogic.xquery

import quasar.Predef._

import java.lang.SuppressWarnings

import scalaz.{Order => _, _}
import scalaz.std.iterable._

@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
object cts {
  def directoryQuery(uri: XQuery, urisOrDepth: XQuery*): XQuery =
    s"cts:directory-query($uri, ${mkSeq(urisOrDepth)})"

  def documentOrder(direction: XQuery): XQuery =
    s"cts:document-order($direction)"

  def indexOrder(index: XQuery, options: XQuery*) =
    s"cts:index-order($index, ${mkSeq(options)})"

  def search(
    expr: XQuery,
    query: XQuery,
    options: IList[XQuery] = IList.empty,
    qualityWeight: Option[XQuery] = None,
    forestIds: IList[XQuery] = IList.empty
  ): XQuery =
    s"cts:search($expr, $query, ${mkSeq(options)}, ${qualityWeight getOrElse "1.0"}, ${mkSeq(forestIds)})"

  val uriReference: XQuery =
    "cts:uri-reference()"

  val uris: XQuery =
    "cts:uris()"

  def uris(start: XQuery, options: IList[XQuery]): XQuery =
    s"cts:uris($start, ${mkSeq(options)})"

  def uris(
    start: XQuery,
    options: IList[XQuery] = IList.empty,
    query: XQuery,
    qualityWeight: Option[XQuery] = None,
    forestIds: IList[XQuery] = IList.empty
  ): XQuery =
    s"cts:uris($start, ${mkSeq(options)}, $query, ${qualityWeight getOrElse "1.0"}, ${mkSeq(forestIds)})"
}
