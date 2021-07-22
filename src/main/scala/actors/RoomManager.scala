package actors
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import websocket.WSMessage

import java.util.UUID
object RoomManager {
  sealed trait Command
  final case class CreateRoom(replyTo: ActorRef[Response]) extends Command
  final case class IncomeWSMessage(message: WSMessage)                 extends Command
  final case object UnsupportedWSMessage                               extends Command
  final case class WSCompleted(roomId: UUID, userId: UUID)             extends Command
  final case class WSFailure(t: Throwable)                             extends Command
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
            val roomHandle: ActorRef[Room.Command] = context.spawn(Room.roomBehavior(Room.RoomData.empty), newUuid.toString)
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