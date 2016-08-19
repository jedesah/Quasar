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
import quasar.Data

import com.marklogic.xcc.types._
import scalaz._

object xdmitem {
  // TODO: What sort of error handling to we want?
  val toData: XdmItem => Data = {
    case item: CtsBox                   => ???
    case item: CtsCircle                => ???
    case item: CtsPoint                 => ???
    case item: CtsPolygon               => ???
    // TODO: What is the difference between JSArray/Object and their *Node variants?
    case item: JSArray                  => ???
    case item: JSObject                 => ???
    case item: JsonItem                 => data.JsonParser.parseFromString(item.asString).getOrElse(Data.NA)
    case item: XdmAttribute             => ???
    // TODO: Inefficient for large data as it must be buffered into memory
    case item: XdmBinary                => Data.Binary(ImmutableArray.fromArray(item.asBinaryData))
    case item: XdmComment               => ???
    case item: XdmDocument              => ???
    case item: XdmElement               => ???
    case item: XdmProcessingInstruction => ???
    case item: XdmText                  => ???
    case item: XSAnyURI                 => ???
    case item: XSBase64Binary           => Data.Binary(ImmutableArray.fromArray(item.asBinaryData))
    case item: XSBoolean                => Data.Bool(item.asPrimitiveBoolean)
    case item: XSDate                   => ???
    case item: XSDateTime               => ???
    case item: XSDecimal                => Data.Dec(item.asBigDecimal)
    case item: XSDouble                 => Data.Dec(item.asBigDecimal)
    case item: XSDuration               => ???
    case item: XSFloat                  => Data.Dec(item.asBigDecimal)
    case item: XSGDay                   => ???
    case item: XSGMonth                 => ???
    case item: XSGMonthDay              => ???
    case item: XSGYear                  => ???
    case item: XSGYearMonth             => ???
    case item: XSHexBinary              => Data.Binary(ImmutableArray.fromArray(item.asBinaryData))
    case item: XSInteger                => Data.Int(item.asBigInteger)
    case item: XSQName                  => ???
    case item: XSString                 => Data.Str(item.asString)
    case item: XSTime                   => ???
    case item: XSUntypedAtomic          => ???
    case _                              => Data.NA // This case should not be hit although we can't prove it.
  }
}
