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

@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
object fn {
  def baseUri(node: XQuery): XQuery =
    s"fn:base-uri($node)"

  def concat(x: XQuery, xs: XQuery*): XQuery =
    s"fn:concat${mkSeq_(x, xs: _*)}"

  def doc(uri: XQuery = ""): XQuery =
    s"fn:doc($uri)"

  def exists(seq: XQuery): XQuery =
    s"fn:exists($seq)"

  val False: XQuery =
    "fn:false()"

  def filter(p: XQuery, seq: XQuery): XQuery =
    s"fn:filter($p, $seq)"

  val last: XQuery =
    "fn:last()"

  def map(f: XQuery, seq: XQuery): XQuery =
    s"fn:map($f, $seq)"

  def not(bool: XQuery): XQuery =
    s"fn:not($bool)"

  def QName(localPart: XQuery): XQuery =
    s"fn:QName($localPart)"

  def QName(namespace: XQuery, localPart: XQuery): XQuery =
    s"fn:QName($namespace, $localPart)"

  def startsWith(str: XQuery, prefix: XQuery, collation: Option[XQuery] = None): XQuery =
    s"fn:starts-with($str, ${prefix}${asArg(collation)})"

  def tokenize(input: XQuery, pattern: XQuery, flags: Option[XQuery] = None): XQuery =
    s"fn:tokenize($input, ${pattern}${asArg(flags)})"

  val True: XQuery =
    "fn:true()"
}
