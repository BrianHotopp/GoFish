package actors
import actors.Room.{AskForRank, Leave}
import actors.RoomManager.createRoomRef
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import websocket.{Ask, PushState, UserJoin, WSMessage}
import akka.actor.{ActorRef => UntypedRef}
import gamedata.DeckOfCards.Deck.{defaultDeck, emptyDeck}
import gamedata.DeckOfCards.{Deck, Rank, Spade, Two}
import gamedata.{GoFish, PlayerData, RoomManagerData}

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
  // should come in to the room manager if the client terminates the connection
  final case class WSFailure(t: Throwable)                             extends Command
  // should come into the room manager if the client has a ws failure
  final case class CompleteWS()                                        extends Command

  final case class ConnectToRoom(message: WSMessage, user: UntypedRef) extends Command
  final case class RoomResponseWrapper(response: Room.Response) extends Command

  sealed trait Response
  final case class RoomId(value: String) extends Response



  def apply(): Behavior[RoomManager.Command] = Behaviors.setup[Command] {
    context=>
      val roomResponderActor = context.messageAdapter(response=>RoomResponseWrapper(response))
      roomManagerBehavior(RoomManagerData.empty, roomResponderActor)
  }
  // only so many things can happen over websocket
  // user joins incoming
  // user leaves incoming
  // user asks incoming
  // state push outgoing

  def roomManagerBehavior(roomManagerData: RoomManagerData, roomResponderActor: ActorRef[Room.Response]): Behavior[RoomManager.Command] = {
    Behaviors.receive(
      onMessage = (context: ActorContext[RoomManager.Command], command: RoomManager.Command) => {
        command match {
          case CreateRoom(replyTo) =>
            // create room call that comes straight from the api
            context.log.info("Room Manager Received Create Room")
            val roomUUID = UUID.randomUUID()
            val roomRef = createRoomRef(roomUUID, context)
            val newData = roomManagerData.addRoom(roomUUID, roomRef)
            context.watch(roomRef)
            replyTo ! RoomId(roomUUID.toString)
            roomManagerBehavior(newData, roomResponderActor)
          case ConnectToRoom(message, userRef) =>
            // doubles as create room if no such room exists
            context.log.info("Room Manager Received Connect To Room")
            roomManagerData.rooms.get(message.roomId).fold{
              // there is no such room, we must
              // make a room and add it to our roommanager's list, make our user and add him to the room
              message match {
                case WSMessage(roomId, userId, UserJoin(name)) =>
                  val roomActor = createRoomRef(roomId, context)
                  val newRoomManagerData = roomManagerData.addRoom(roomId, roomActor)
                  context.watch(roomActor)
                  val newUser = PlayerData(userId, Some(userRef), name, Some(defaultDeck), 0, List())
                  // add our new user to our new room
                  roomActor ! Room.Join(newUser)
                  roomManagerBehavior(newRoomManagerData, roomResponderActor)
                case _ =>
                  roomManagerBehavior(roomManagerData, roomResponderActor)
              }
            }{
              roomActor =>
                message match {
                  case WSMessage(roomId, userId, UserJoin(name)) =>
                    val newRoomManagerData = roomManagerData.addRoom(roomId, roomActor)
                    context.watch(roomActor)
                    val newUser = PlayerData(userId, Some(userRef), name, Some(emptyDeck), 0, List())
                    // add our new user to our new room
                    roomActor ! Room.Join(newUser)
                    roomManagerBehavior(newRoomManagerData, roomResponderActor)
                  case _ =>
                    roomManagerBehavior(roomManagerData, roomResponderActor)
            }}
          case IncomeWSMessage(message) =>
            // room specific message just gets sent to room
            // todo handle exception
            val room = roomManagerData.rooms(message.roomId)
            handleIncomingMessage(room, roomResponderActor, message, context)
            roomManagerBehavior(roomManagerData, roomResponderActor)
          case WSCompleted(roomId, userId) =>
            context.log.info("Room Manager Received WSCompleted")
            roomManagerBehavior(roomManagerData, roomResponderActor)
          case WSFailure(t) =>
            context.log.info("Room Manager Received WSFailure")
            roomManagerBehavior(roomManagerData, roomResponderActor)
        }
      }
    )
  }
  def createRoomRef(
                                roomId: UUID,
                                context: ActorContext[Command]
                                ): ActorRef[Room.Command] = {
    // gives back a room in the current context with a name corresponding to the passed in UUID
    context.spawn(actors.Room(roomId), roomId.toString)
  }
  def handleIncomingMessage(room: ActorRef[Room.Command],roomManagerRefToReplyTo: ActorRef[Room.Response], message: WSMessage, context: ActorContext[Command]) : Unit = {
    // room is a reference to the room which will handle the message,
    // roomManagerToReplyTo is an actorRef the roommanager has to accept room status messages
    // message is the message to handle
    // context is a handle to the roomManager itself
    message match {
      // should not be used because it can only come from materializing the user session on the backend
      case WSMessage(roomId, userId, Ask(askeeId, rank)) =>
        room ! AskForRank(userId, askeeId, rank)
      case _ => ()
    }
  }
}
