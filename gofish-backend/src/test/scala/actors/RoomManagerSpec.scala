package actors

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}
class RoomManagerSpec extends FlatSpec with Matchers with BeforeAndAfterAll {
  val testKit: ActorTestKit = ActorTestKit()
  val user1Name = "user 1"
  val user2Name = "user 2"
  override def afterAll(): Unit = {
    testKit.shutdownTestKit()
  }

  "RoomManager Actor" should
    "create room" in {
    val managerRef = testKit.spawn(RoomManager())
    val sender = testKit.createTestProbe[RoomManager.Response]()
    managerRef ! RoomManager.CreateRoom(sender.ref)
    sender.expectMessageType[RoomManager.RoomId]
  }
  it should "connect to a room" in {

  }
}




