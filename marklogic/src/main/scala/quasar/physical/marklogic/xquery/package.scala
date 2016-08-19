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

import scalaz._
import scalaz.std.string._
import scalaz.std.iterable._
import scalaz.syntax.foldable._
import scalaz.syntax.std.option._

package object xquery {
  // TODO: Make a value class at least
  type XQuery = String

  def asArg(opt: Option[XQuery]): String =
    opt.map(", " + _).orZero

  def mkSeq[F[_]: Foldable](fa: F[XQuery]): XQuery =
    s"(${fa.intercalate(", ")})"

  def mkSeq_(x: XQuery, xs: XQuery*): XQuery =
    mkSeq(x +: xs)
}
