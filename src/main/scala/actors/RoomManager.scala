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
  def roomManagerBehavior(roomData: RoomManagerData): Behavior[RoomManager.Command] = {
    Behaviors.receive(
      onMessage = (context: ActorContext[RoomManager.Command], command: RoomManager.Command) => {
        command match {
          case CreateRoom(replyTo) =>
            context.log.info("Room Manager Received Create Room")
            val newUuid = UUID.randomUUID()
            val roomHandle: ActorRef[Room.Command] = context.spawn(Room.roomBehavior(RoomData.), newUuid.toString)
            replyTo ! RoomId(newUuid.toString)
            roomManagerBehavior(roomData.addRoom(newUuid, roomHandle))
          case ConnectToRoom(message, user) =>
            context.log.info("Room Manager Received Connect To Room")
            val newUuid = UUID.randomUUID()
            roomData.rooms.get(message.roomId).fold{
              val roomActor = createRoom(message.roomId, context)
              context.watch(roomActor)
              val newData = roomData.addRoom(message.roomId, roomActor)
              roomActor ! Room.Join(
              )
            }
          case IncomeWSMessage()

            roomManagerBehavior(roomData.removeRoom(roomId))
        }
      }
    )
  }
  private[actors] def handleIncomingMessage(room: ActorRef[Room.Command], message: WSMessage, context: ActorContext[Command]) : Unit = {
    message.messageType match {
      case WSMessage.WSMessageType.Join =>
      case WSMessage.WSMessageType.Ask =>
      case WSMessage.WSMessageType.Leave =>
    }
  }
}
