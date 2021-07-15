package actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import java.util.UUID

object WebSocketUser {
  sealed trait Command
  final case class RoomManagerResponse(message: String) extends Command
  final case class RoomResponse(message: String) extends Command
  case class UserData(uuid: UUID, name: String)
  object UserData {
    val dummyUser = UserData(UUID.randomUUID(), "Dummy User")
  }
  def apply(){
    webSocketUserBehavior(UserData.dummyUser)
  }
  def webSocketUserBehavior(userData: UserData): Behavior[WebSocketUser.Command] = {
    webSocketUserBehavior(userData)
  }


}
