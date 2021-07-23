package actors
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import gamedata.DeckOfCards.{Deck, Rank}
import gamedata.PlayerData
import websocket.WSMessage
import websocket.WSMessage.MessageType
import gamedata.RoomData
import java.util.UUID

object Room {
  sealed trait Command
  final case class Join(playerId: UUID) extends Command
  final case class Leave(playerId: UUID, replyTo: ActorRef[Response]) extends Command
  final case class AskForRank(askerId: UUID, askeeId: UUID, rank: Rank) extends Command
  final case class GetGameState(askerId: UUID)
  sealed trait Response
  final case class Running(roomId: UUID) extends Response
  final case class Stopped(roomId: UUID) extends Response


  def apply(): Behavior[Room.Command] ={
    roomBehavior(RoomData.initial)
  }
  private[actors] def broadcast(
                                 message: WSMessage,
                                 users: List[PlayerData],
                                 context: ActorContext[Command]
                               ): Unit = {
    context.log.debug("Broadcasting: {} ", message)
    users.foreach { user =>
      user.ref ! message
    }
  }

  private[actors] def setupNewUser(player: PlayerData, roomId: UUID, data: RoomData): Unit = {
    // broadcasts to the vue client that it should add a new user to its room's list of users
    // and also set its personal "user" to the uuid of the new player
    player.ref ! WSMessage(MessageType.Init, roomId, player.id, player.name)

    data.players.foreach { u =>
      player.ref ! WSMessage(MessageType.Join, roomId, u.id, u.name)
    }
  }
  def roomBehavior(data: RoomData): Behavior[Room.Command] ={
    Behaviors.receive(
      (context: ActorContext[Room.Command], command: Room.Command) => {
        command match {

        }
      }
    )
  }

  def broadcast(message: WSMessage, users: List[PlayerData], context: ActorContext[Command]): Unit ={
    // send WSmessage to the session actor of al lthe players in the passed in list
    users.foreach(
      user=>user.ref ! message
    )

  }
}
