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

import scalaz.std.iterable._

@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
object xdmp {
  def directory(uri: XQuery, depth: XQuery): XQuery =
    s"xdmp:directory($uri, $depth)"

  def directoryCreate(uri: XQuery): XQuery =
    s"xdmp:directory-create($uri)"

  def directoryDelete(uri: XQuery): XQuery =
    s"xdmp:directory-delete($uri)"

  def directoryProperties(uri: XQuery, depth: XQuery): XQuery =
    s"xdmp:directory-properties($uri, $depth)"

  def documentDelete(uri: XQuery): XQuery =
    s"xdmp:document-delete($uri)"

  def documentGetProperties(uri: XQuery, property: XQuery): XQuery =
    s"xdmp:document-get-properties($uri, $property)"

  def documentInsert(uri: XQuery, root: XQuery): XQuery =
    s"xdmp:document-insert($uri, $root)"

  def documentProperties(uris: XQuery*): XQuery =
    s"xdmp:document-properties${mkSeq(uris)}"

  def nodeUri(node: XQuery): XQuery =
    s"xdmp:node-uri($node)"
}
