package gamedata

import actors.Room
import akka.actor.typed.ActorRef

import java.util.UUID

final case class RoomManagerData(rooms: Map[UUID, ActorRef[Room.Command]]) {
  def addRoom(roomId: UUID, roomActor: ActorRef[Room.Command]): RoomManagerData = {
    this.copy(rooms = this.rooms + (roomId -> roomActor))
  }
  def removeRoom(roomId: UUID): RoomManagerData = {
    this.copy(rooms = this.rooms - roomId)
  }
}

object RoomManagerData {
  val empty: RoomManagerData = RoomManagerData(rooms = Map.empty[UUID, ActorRef[Room.Command]])
}
