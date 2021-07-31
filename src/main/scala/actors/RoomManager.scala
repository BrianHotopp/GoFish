package actors
import actors.Room.{AskForRank, Leave}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import websocket.WSMessage
import akka.actor.{ActorRef => UntypedRef}
import gamedata.DeckOfCards.Deck.{defaultDeck, emptyDeck}
import gamedata.DeckOfCards.{Deck, Rank, Spade, Two}
import gamedata.{GoFish, PlayerData, RoomManagerData}
import websocket.WSMessage.{Ask, UserJoin}

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
  final case class RoomResponseWrapper(response: Room.Response) extends Command

  sealed trait Response
  final case class RoomId(value: String) extends Response



  def apply(): Behavior[RoomManager.Command] = Behaviors.setup[Command] {
    context=>
      val roomResponderActor = context.messageAdapter(response=>RoomResponseWrapper(response))
      roomManagerBehavior(RoomManagerData.empty, roomResponderActor)
  }
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
              // make a room and add it to our roommanager's list
              // add our user to that room

              // we make a room with two pieces: a new roomId specified (the one that didn't exist) and a new roomActor we generate
              // we must add the room to our room manager data and pass that to the next behavior
              val roomActor = createRoomRef(message.roomId, context)
              val newRoomManagerData = roomManagerData.addRoom(message.roomId, roomActor)
              context.watch(roomActor)
              // now we must make a user to add to the room we just made, we have the ref
              // passed in by the message from the client, but we must make a new uuid and create the player
              // we also need the name passed in in the wsmessage from the user
              val name = message.payload match {
                case UserJoin(name) => name
                case _ =>
                  context.log.error("ERROR: got something other than UserJoin payload in a WSMessage part of a command of type Join")
                  "INVALID_USERNAME"
              }
              // create new user with name from wsmessage, ref from connecttoroom (from our backend session creation in wsmessage), and random uuid
              val newUser = PlayerData(UUID.randomUUID(), Some(userRef), name, Some(defaultDeck), 0, List())
              roomActor ! Room.Join(newUser)
              roomManagerBehavior(newRoomManagerData, roomResponderActor)
            }{
              roomActor =>
                val newRoomManagerData = roomManagerData.addRoom(message.roomId, roomActor)
              message.payload match {
                case UserJoin(name) =>
                  // create new user with name from wsmessage, ref from connecttoroom (from our backend session creation in wsmessage), and random uuid

                  val newUser = PlayerData(UUID.randomUUID(), Some(userRef), name, Some(emptyDeck), 0, List())
                  roomActor ! Room.Join(newUser)
                  roomManagerBehavior(newRoomManagerData, roomResponderActor)
                case _ =>
                  context.log.error("ERROR: got something other than UserJoin payload in a WSMessage part of a command of type Join")
                  Behaviors.same
              }
            }
          case IncomeWSMessage(message) =>
            // todo handle exception
            val room = roomManagerData.rooms(message.roomId)
            handleIncomingMessage(room, message, context, roomResponderActor)
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
  def handleIncomingMessage(room: ActorRef[Room.Command], message: WSMessage, context: ActorContext[Command], roomManagerRefToReplyTo: ActorRef[Room.Response]) : Unit = {
    // room is a reference to the room which will handle the message, message is the message, context is a handle to the roomManager
    message.messageType match {
      case WSMessage.WSMessageType.Join =>
       // should not be used because it can only come from materializing the user session on the backend
      case WSMessage.WSMessageType.Ask =>
        // unpack message payload
        val payload = message.payload match {
          case a: Ask => a
          case _ => Ask(UUID.randomUUID(), Two)
        }
        room ! AskForRank(message.userId, payload.askeeId, payload.rank)
      case WSMessage.WSMessageType.Leave =>
        // need to get userid and
        room ! Leave(message.roomId, roomManagerRefToReplyTo)
    }
  }
}
