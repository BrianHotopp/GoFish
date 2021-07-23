package actors
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import websocket.WSMessage
import akka.actor.{ActorRef => UntypedRef}
import gamedata.{RoomData, RoomManagerData}

import java.util.UUID
object RoomManager {
  sealed trait Command
  // this can come in straight from the api
  final case class CreateRoom(replyTo: ActorRef[Response]) extends Command

  // this is a ws message from the client
  final case class IncomeWSMessage(message: WSMessage)                 extends Command
  // comes in from the client websocket stream when the websocket connection is closed
  final case object UnsupportedWSMessage                               extends Command
  // comes in from the client websocket if it fails to make a connection to the server
  final case class WSCompleted(roomId: UUID, userId: UUID)             extends Command
  // should goe out to the client ref if some server process fails
  final case class WSFailure(t: Throwable)                             extends Command
  // should go out to the client ref if the server is done processing the requets and wishes to terminate the session
  final case class CompleteWS()                                        extends Command

  final case class ConnectToRoom(message: WSMessage, user: UntypedRef) extends Command
  final case class RoomResponseWrapper(response: RoomManager.Response) extends Command

  sealed trait Response
  final case class RoomId(value: String) extends Response



  def apply(): Behavior[RoomManager.Command] = roomManagerBehavior(RoomManagerData.empty)
  def roomManagerBehavior(roomData: RoomManagerData): Behavior[RoomManager.Command] =
    Behaviors.receive(
      onMessage = (context: ActorContext[RoomManager.Command], command: RoomManager.Command) => {
        command match {
          case CreateRoom(replyTo) =>
            context.log.info("Room Manager Received Create Room")
            val newUuid = UUID.randomUUID()
            val roomHandle: ActorRef[Room.Command] = context.spawn(Room.roomBehavior(RoomData.), newUuid.toString)
            replyTo ! RoomId(newUuid.toString)
            roomManagerBehavior(roomData.addRoom(newUuid, roomHandle))
          case DeleteRoom(roomId) =>
            context.log.info(s"Room Manager Received Delete Room UUID: $roomId")
            // get the reference to the room we want to stop from the manager's map of UUID->Refs
            val stopRef = roomData.rooms.get(roomId)
            stopRef match {
              case Some(ref) =>
                context.stop(ref: ActorRef[Room.Command])
              case None =>
                context.log.info(s"Received command to stop actor with uuid $roomId but roomData does not contain an entry with that Id; room could not be halted")
            }
            roomManagerBehavior(roomData.removeRoom(roomId))
        }
      }
    )
}
