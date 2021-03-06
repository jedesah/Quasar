/*
 * Copyright 2014–2018 SlamData Inc.
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

package quasar.physical.mongodb

import scala.annotation.unchecked.uncheckedVariance

import com.mongodb.async.client.MongoIterable
import com.mongodb.{Function => MFunction}
import scalaz._, Liskov.<~<

object mongoiterable {
  final implicit class MongoIterableOps[A](val self: MongoIterable[A]) extends scala.AnyVal {
    def widen[B](implicit ev: A <~< B): MongoIterable[B] =
      ev.subst[λ[`-X` => MongoIterable[X @uncheckedVariance] <~< MongoIterable[B]]](Liskov.refl)(self)
  }

  implicit val mongoIterableFunctor: Functor[MongoIterable] =
    new Functor[MongoIterable] {
      def map[A, B](ma: MongoIterable[A])(f: A => B) = {
        val fn = new MFunction[A, B] {
          def apply(a: A) = f(a)
        }
        ma map fn
      }
    }
}
