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

package quasar.physical.mongodb.fs

import quasar.Predef._
import quasar._
import quasar.fs.{ADir, FileSystemType}
import quasar.fs.mount.{ConnectionUri, MountConfig}, MountConfig._
import quasar.physical.mongodb._

import com.mongodb.async.client.MongoClient
import org.specs2.specification.core.Fragments
import pathy.Path._
import scalaz._, Scalaz._
import scalaz.concurrent.Task

class MongoDbIOSpec extends QuasarSpecification {
  def cfgs(fsType: FileSystemType): Task[List[(BackendName, ConnectionUri, ConnectionUri)]] =
    TestConfig.backendNames.traverse(n => TestConfig.loadConfigPair(n).map[Option[(BackendName, ConnectionUri, ConnectionUri)]] {
      case (FileSystemConfig(`fsType`, testUri),
            FileSystemConfig(`fsType`, setupUri)) =>
        (n, testUri, setupUri).some
      case _ => None
    }.run).map(_.map(_.join).flatten)

  def connect(uri: ConnectionUri): Task[MongoClient] =
    asyncClientDef[Task](uri).run.foldMap(NaturalTransformation.refl).flatMap(_.fold(
      err => Task.fail(new RuntimeException(err.toString)),
      Task.now))

  def clientShould(examples: (ADir, MongoClient, MongoClient) => Fragments): Unit =
    TestConfig.testDataPrefix.flatMap { prefix =>
      cfgs(MongoDBFsType).map(_ traverse_[Id] { case (name, setupUri, testUri) =>
        (connect(setupUri) |@| connect(testUri)) { (setupClient, testClient) =>
          s"${name.name}" should examples(prefix, setupClient, testClient)

          step(testClient.close)

          ()
        }.unsafePerformSync
      })
    }.unsafePerformSync

  clientShould { (prefix, setupClient, testClient) =>
    import MongoDbIO._

    val tempColl: Task[Collection] =
      for {
        n <- NameGenerator.salt
        c <- Collection.fromFile(prefix </> file(n))
              .fold(err => Task.fail(new RuntimeException(err.shows)), Task.now)
      } yield c

    "get mongo version" in {
      serverVersion.run(testClient).unsafePerformSync.length must beGreaterThanOrEqualTo(2)
    }

    "get stats" in {
      (for {
        coll  <- tempColl
        _     <- insert(
                  coll,
                  List(Bson.Doc(ListMap("a" -> Bson.Int32(0)))).map(_.repr)).run(setupClient)
        stats <- collectionStatistics(coll).run(testClient)
        _     <- dropCollection(coll).run(setupClient)
      } yield {
        stats.count    must_=== 1
        stats.dataSize must beGreaterThan(0L)
        stats.sharded  must beFalse
      }).unsafePerformSync
    }
  }
}
