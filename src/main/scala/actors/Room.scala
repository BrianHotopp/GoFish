package actors
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import gamedata.DeckOfCards.{Deck, Rank}
import gamedata.{GoFish, PlayerData, RoomData}
import websocket.WSMessage
import websocket.WSMessage.WSMessageType

import java.util.UUID
import akka.actor.ActorRef

import scala.::
object Room {
  sealed trait Command
  final case class Join(player: PlayerData) extends Command
  final case class Leave(playerId: UUID, replyTo: ActorRef[Response]) extends Command
  final case class AskForRank(askerId: UUID, askeeId: UUID, rank: Rank) extends Command
  final case class GetGameState(askerId: UUID)
  sealed trait Response
  final case class Running(roomId: UUID) extends Response
  final case class Stopped(roomId: UUID) extends Response


  def apply(): Behavior[Room.Command] ={
    Behaviors.setup[Command] {
      _ =>
        roomBehavior(GoFish(List(), Some(Deck())))
    }
  }
  private[actors] def pushState(
                               game_state: GoFish,
                               context: ActorContext[Command]
                               ): Unit ={
    // sends a masked version of the gamestate to each player in the passed in gamestate
    val users = game_state.players
    users.foreach(
      // mask the things the user is not allowed to see

    )
  }



  def roomBehavior(data: GoFish): Behavior[Room.Command] ={
    Behaviors.receive(
      (context: ActorContext[Room.Command], command: Room.Command) => {
        command match {
          case Join(user) =>
            // adds the passed in user to the room's data
            roomBehavior(data.copy(players = user :: data.players))
          case Leave(userId, roomToReplyTo) =>
            val newData = data.removePlayer(userId)
            // broadcast the leave to all players in the room
            // if this leave made the game data have no players
            // send a stopped message to the RoomManager and do Behaviors.Stopped
            // else
            if (newData.users.isEmpty) {
              replyTo ! Stopped(roomId)
              Behaviors.stopped
            } else {
              replyTo ! Running(roomId)
              receiveBehaviour(roomId, newData)
            }
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
