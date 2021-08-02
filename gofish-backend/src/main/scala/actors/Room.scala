package actors
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import gamedata.DeckOfCards.Deck.{defaultDeck, emptyDeck}
import gamedata.DeckOfCards.{Deck, Rank}
import gamedata.GoFish.{Waiting, goFishStart}
import gamedata.{GoFish, PlayerData}
import websocket.{PushState, WSMessage}

import java.util.UUID
import scala.::
object Room {
  sealed trait Command
  final case class Join(player: PlayerData) extends Command
  final case class Leave(playerId: UUID, replyTo: ActorRef[Response]) extends Command
  final case class AskForRank(askerId: UUID, askeeId: UUID, rank: Rank) extends Command
  final case class DealInPlayer(toDealInId: UUID) extends Command
  final case class ReadyPlayer(toReadyId: UUID) extends Command
  final case object StartGame extends Command
  sealed trait Response
  final case class Running(roomId: UUID) extends Response
  final case class Stopped(roomId: UUID) extends Response

  case class RoomData(roomId: UUID, gameData: GoFish)

  def apply(uuid: UUID): Behavior[Room.Command] ={
    Behaviors.setup[Command] {
      _ =>
        roomBehavior(RoomData(uuid, goFishStart))
    }
  }

  val MINPLAYERS = 2
  val CARDSPERPLAYER = 7

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
          case ReadyPlayer(toReadyId) =>
            // ready the player passed in
            val newGameData = data.gameData.copy(players = data.gameData.players.map(x=>if(x.id == toReadyId){x.copy(readied = true)}else{x}))
            val newRoomData = data.copy(gameData = newGameData)
            pushState(newRoomData)
            roomBehavior(newRoomData)
          case DealInPlayer(toDealInId) =>
            val dealAttempt = data.gameData.dealToOne(CARDSPERPLAYER, toDealInId)
            dealAttempt match {
              case Some(dealtGameData) =>
                // ready the player we just dealt in
                val newGameData = dealtGameData.copy(players = dealtGameData.players.map(x=>if(x.id == toDealInId){x.copy(readied = true)}else{x}))
                val newRoomData = data.copy(gameData = newGameData)
                roomBehavior(newRoomData)
              case None => Behaviors.same // we were unable to deal the cards to the player
            }
          case StartGame =>
            if(data.gameData.players.size > MINPLAYERS && data.gameData.allPlayersReady()){
              // deal to all players, set first turn, set game status to running
              val newGameData = data.gameData.dealToAll(CARDSPERPLAYER).get.copy(turn = Some(data.gameData.players(0).id), gameStatus = GoFish.Running)
              val newRoomData = data.copy(gameData = newGameData)
              pushState(newRoomData)
              roomBehavior(data = newRoomData)
            }else{
              Behaviors.same
            }

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
        user.ref.get ! WSMessage(roomData.roomId, user.id, PushState(maskedState))
      }
      // make a masked version of the gamestate

    )

  }
}
