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

package quasar.physical.marklogic.xcc

import quasar.Predef._
import quasar.SKI._
import quasar.fp.numeric.Positive

import scala.collection.JavaConverters._

import com.marklogic.xcc._
import com.marklogic.xcc.types.XdmItem
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.{Process, io}

// TODO: Investigate sharing content source for multiple content bases
final class ContentSourceIO[A] private (protected val k: Kleisli[Task, ContentSource, A]) {
  def map[B](f: A => B): ContentSourceIO[B] =
    new ContentSourceIO(k map f)

  def flatMap[B](f: A => ContentSourceIO[B]): ContentSourceIO[B] =
    new ContentSourceIO(k flatMap (a => f(a).k))

  def attempt: ContentSourceIO[Throwable \/ A] =
    new ContentSourceIO(k mapK (_.attempt))

  def run(cs: ContentSource): Task[A] =
    k.run(cs)
}

object ContentSourceIO {

  def fail[A](t: Throwable): ContentSourceIO[A] =
    liftT(Task.fail(t))

  val liftT: Task ~> ContentSourceIO =
    new (Task ~> ContentSourceIO) {
      def apply[A](ta: Task[A]) = lift(κ(ta))
    }

  def resultCursor(qr: SessionIO[QueryResults], chunkSize: Positive): ContentSourceIO[ResultCursor] =
    newSession(some(streamingOptions)) flatMap { s =>
      liftT(qr.run(s)) map (r => new ResultCursor(chunkSize, s, r.resultSequence))
    }

  def resultStream(qr: SessionIO[QueryResults]): Process[ContentSourceIO, XdmItem] = {
    def closeSession(s: Session) =
      Process.eval_(liftT(Task.delay(s.close())))

    def itemStream(s: Session) =
      io.iterator(qr.run(s) map (_.resultSequence.iterator.asScala))
        .flatMap(ritem => Process.eval(resultitem.loadItem(ritem)))
        .translate(liftT)

    Process.bracket(newSession(some(streamingOptions)))(closeSession)(itemStream)
  }

  def runNT(cs: ContentSource): ContentSourceIO ~> Task =
    new (ContentSourceIO ~> Task) {
      def apply[A](csio: ContentSourceIO[A]) = csio.run(cs)
    }

  val runSessionIO: SessionIO ~> ContentSourceIO =
    new (SessionIO ~> ContentSourceIO) {
      def apply[A](sio: SessionIO[A]) =
        newSession_ flatMap { session =>
          liftT(sio.run(session).onFinish(κ(Task.delay(session.close))))
        }
    }

  implicit val contentSourceIOInstance: Monad[ContentSourceIO] with Catchable[ContentSourceIO] =
    new Monad[ContentSourceIO] with Catchable[ContentSourceIO] {
      override def map[A, B](fa: ContentSourceIO[A])(f: A => B) = fa map f
      def point[A](a: => A) = liftT(Task.now(a))
      def bind[A, B](fa: ContentSourceIO[A])(f: A => ContentSourceIO[B]) = fa flatMap f
      def fail[A](t: Throwable) = ContentSourceIO.fail(t)
      def attempt[A](fa: ContentSourceIO[A]) = fa.attempt
    }

  ////

  private def apply[A](f: ContentSource => A): ContentSourceIO[A] =
    lift(cs => Task.delay(f(cs)))

  private def lift[A](f: ContentSource => Task[A]): ContentSourceIO[A] =
    new ContentSourceIO(Kleisli(f))

  private def newSession(defaultRequestOptions: Option[RequestOptions]): ContentSourceIO[Session] =
    apply { cs =>
      val session = cs.newSession
      defaultRequestOptions foreach session.setDefaultRequestOptions
      session
    }

  private def newSession_ : ContentSourceIO[Session] =
    newSession(None)

  private def streamingOptions: RequestOptions = {
    val opts = new RequestOptions
    opts.setCacheResult(false)
    opts
  }
}
