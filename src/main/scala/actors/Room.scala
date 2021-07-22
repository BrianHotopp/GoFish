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
    player.ref ! WSMessage(MessageType.Init, roomId, player.id, player.name)

    data.players.foreach { u =>
      player.ref ! WSMessage(MessageType.Join, roomId, u.id, u.name)
    }
  }
  def roomBehavior(data: RoomData): Behavior[Room.Command] ={
    Behaviors.receive(
      (context: ActorContext[Room.Command], command: Room.Command) => {
        command match {
          case Join(playerId) =>
            context.log.info(s"Room ${context.self.toString} received Join Message with playerId: $playerId")
            roomBehavior(data.addPlayer(playerId))
          case Leave(playerId, replyTo) =>
            context.log.info(s"Room ${context.self.toString} received Leave Message with playerId: $playerId")
            roomBehavior(data.deletePlayer(playerId))
          case AskForRank(askerId, askeeId, rank) =>
            context.log.info(s"Room ${context.self.toString} received AskForRank Message with player $askerId asking $askeeId for ${rank.toString}")
            val newData = data.Ask
            roomBehavior()
          case GetGameState(askerId) =>
            Behaviors.same
        }
      }
    )
  }
}