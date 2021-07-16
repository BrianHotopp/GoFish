package actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import java.util.UUID

object WebSocketUser {
  sealed trait Command
  final case class RoomManagerResponse(message: String) extends Command
  final case class RoomResponse(message: String) extends Command
  // message received from websocket flow when the websocket stream is initialized
  final case class StreamInitialized() extends Command
  // message received from websocket flow when the websocket stream is terminated
  final case class StreamTerminated() extends Command
  // message received from websocket flow when the websocket stream errors out
  final case class StreamError(msg: Throwable) extends Command
  case class UserData(uuid: UUID, name: String){
    def getUserName: String= name
    def getUuid:UUID = uuid
  }
  object UserData {
    val dummyUser = UserData(UUID.randomUUID(), "Dummy User")
  }
  def apply(userName: String): Behavior[WebSocketUser.Command] = {
    webSocketUserBehavior(UserData(UUID.randomUUID(), userName))
  }
  def webSocketUserBehavior(userData: UserData): Behavior[WebSocketUser.Command] = {
    webSocketUserBehavior(userData)
  }


}
