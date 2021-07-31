package actors
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import gamedata.DeckOfCards.Deck.defaultDeck
import gamedata.DeckOfCards.{Deck, Rank}
import gamedata.{GoFish, PlayerData}
import websocket.WSMessage
import websocket.WSMessage.{PushState, WSMessageType}

import java.util.UUID
import scala.::
object Room {
  sealed trait Command
  final case class Join(player: PlayerData) extends Command
  final case class Leave(playerId: UUID, replyTo: ActorRef[Response]) extends Command
  final case class AskForRank(askerId: UUID, askeeId: UUID, rank: Rank) extends Command
  sealed trait Response
  final case class Running(roomId: UUID) extends Response
  final case class Stopped(roomId: UUID) extends Response

  case class RoomData(roomId: UUID, gameData: GoFish)

  def apply(uuid: UUID): Behavior[Room.Command] ={
    Behaviors.setup[Command] {
      _ =>
        roomBehavior(RoomData(uuid, GoFish(List(), Some(defaultDeck), turn = UUID.randomUUID())))
    }
  }

  def roomBehavior(data: RoomData): Behavior[Room.Command] ={
    Behaviors.receive[Command](
      (context, command) => {
        command match {
          case Join(user) =>
            // add the passed in user to the game data
            // broadcast the new state to all users in the room
            val newGameData =  data.gameData.copy(players = user::data.gameData.players)
            val newRoomData = data.copy(gameData = newGameData)
            pushState(newRoomData)
            roomBehavior(newRoomData)
          case AskForRank(askerId, askeeId, rank) =>
            val newGameData = data.gameData.askForCard(rank, askerId, askeeId)
            val newRoomData = data.copy(gameData = newGameData)
            pushState(newRoomData)
            Behaviors.same
          case Leave(userId, roomRefToReplyTo) =>
            val newGameData = data.gameData.removePlayerById(userId)
            val newRoomData = data.copy(gameData = newGameData)
            // broadcast the leave to all players in the room
            pushState(newRoomData)
            // if this leave made the game data have no players
            // send a stopped message to the RoomManager and do Behaviors.Stopped
            // else
            if (newGameData.players.isEmpty) {
              roomRefToReplyTo ! Stopped(data.roomId)
              Behaviors.stopped
            } else {
              roomRefToReplyTo ! Running(data.roomId)
              roomBehavior(newRoomData)
            }
        }

      }
    )
  }

  def pushState(roomData: RoomData):  Unit ={
    // send WSmessage to the session actor of al lthe players in the passed in list
    val state = roomData.gameData
    val users = state.players
    users.foreach(
      user => {
        val maskedState = state.mask(user.id)
        // todo handle the fact that this can err
        user.ref.get ! WSMessage(WSMessageType.PushState, roomData.roomId, user.id, PushState(maskedState))
      }
      // make a masked version of the gamestate

    )

  }
}
