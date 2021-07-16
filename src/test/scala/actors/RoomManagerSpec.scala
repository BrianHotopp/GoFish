package actors

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

class RoomManagerSpec
  extends FlatSpec
    with BeforeAndAfterAll
    with Matchers {
  val testKit = ActorTestKit()
  // make sure the testing actor system gets shut down after all the test are run
  override def afterAll(): Unit = testKit.shutdownTestKit()
  it should "create a room and return a room UUID"
  val pinger = testKit.spawn(RoomManager(), "manager1")
  val probe = testKit.createTestProbe[RoomManager.Command]()
  //pinger !
  //probe.expectMessage(Echo.Pong("hello"))
}


