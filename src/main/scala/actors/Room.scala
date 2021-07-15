package actors
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}

import java.util.UUID

object Room {
  sealed trait Command
  final case class AddPlayer(playerId: UUID) extends Command
  final case class RemovePlayer(playerId: UUID) extends Command
  final case class ModifyState(number: Int, option: String) extends Command
  final case class RoomData(players: List[UUID], count: Int){
    def addPlayer(playerId: UUID): RoomData = {
      this.copy(players = playerId :: players)
    }
    def deletePlayer(playerId: UUID): RoomData = {
      this.copy(players = players.filter(x=>x!=playerId))
    }
    def addNumber(number: Int): RoomData = {
      this.copy(count=number+1)
    }
    def subtractNumber(number: Int): RoomData = {
      this.copy(count=number-1)
    }
  }
  object RoomData {
    val empty: RoomData = RoomData(List(), 0)
  }
  def apply(): Behavior[Room.Command] ={
    roomBehavior(RoomData.empty)
  }
  def roomBehavior(data: RoomData): Behavior[Room.Command] ={
    Behaviors.receive(
      (context: ActorContext[Room.Command], command: Room.Command) => {
        command match {
          case AddPlayer(playerId) =>
            context.log.info(s"Room ${context.self.toString} received AddPlayer Message")
            roomBehavior(data.addPlayer(playerId))
          case RemovePlayer(playerId) =>
            context.log.info(s"Room ${context.self.toString} received RemovePlayer Message")
            roomBehavior(data.deletePlayer(playerId))
          case ModifyState(number, option) =>
            context.log.info(s"Room ${context.self.toString} received ModifyState Message")
            if(option == "+"){
              roomBehavior(data.addNumber(number))
            }else{
              roomBehavior(data.subtractNumber(number))
            }
        }
      }
    )
  }
}