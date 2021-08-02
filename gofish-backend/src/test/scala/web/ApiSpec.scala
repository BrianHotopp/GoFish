package web

import actors.RoomManager

import java.util.UUID
import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, SpawnProtocol}
import akka.http.scaladsl.server._
import com.typesafe.config.ConfigFactory
import config.ApiConfig
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import scala.io.Source

class APISpec
  extends FlatSpec
    with Matchers
    with ScalatestRouteTest
    with BeforeAndAfterAll {

  val apiConfig: ApiConfig = ApiConfig.load(ConfigFactory.load())
  val roomId: String = UUID.randomUUID().toString

  val testKit: ActorTestKit = ActorTestKit()
  val roomManager: ActorRef[RoomManager.Command] =
    testKit.spawn(Behaviors.receiveMessagePartial[RoomManager.Command] {
      case RoomManager.CreateRoom(replyTo) =>
        replyTo ! RoomManager.RoomId(roomId)
        Behaviors.same
    })
  implicit val typedSystem: ActorSystem[SpawnProtocol.Command] =
    ActorSystem(Behaviors.setup[SpawnProtocol.Command](_ => SpawnProtocol()), "pointing-poker")

  val apiRoute: Route = API(roomManager, apiConfig).route

  override def afterAll(): Unit = {
    super.afterAll()
    testKit.shutdownTestKit()
    typedSystem.terminate()
  }

  "API" should "return correct response when hitting the root" in {
    Get() ~> apiRoute ~> check {
      responseAs[String] shouldBe "<h1>Hit the API Root</h1>"
    }
  }
  it should "give a room UUID when hitting /create-room" in {
    Post("/create-room") ~> apiRoute ~> check {
      responseAs[String] shouldBe roomId
    }
  }


}
