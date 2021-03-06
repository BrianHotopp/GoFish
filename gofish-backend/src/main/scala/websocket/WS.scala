package websocket

import actors.RoomManager
import akka.NotUsed
import akka.actor.ActorRef
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.{CompletionStrategy, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Sink, Source}
import play.api.libs.json.Json
import java.util.UUID

object WS {
  // websocket stream object

  val disabledBufferSize = 0

  def handler(roomId: UUID, name: String, roomManager: ActorRef): Flow[Message, Message, Any] = {
    // takes a name because we want to get the name when we materialize the flow later
    val userId = UUID.randomUUID()
    Flow.fromSinkAndSource[Message, Message](
      sink(roomManager, roomId, userId),
      source(roomManager, roomId, userId, name)
    )
  }

  private def sink(roomManager: ActorRef, roomId: UUID, userId: UUID): Sink[Message, NotUsed] =
    Sink
      .actorRef(
        roomManager,
        RoomManager.WSCompleted(roomId, userId),
        failure => RoomManager.WSFailure(failure)
      )
      .contramap {
            // if we get a message, convert it to a WSMessage through some json parting
            // todo figure out the format of the websocket message by playing with json.parse
        case TextMessage.Strict(body) => RoomManager.IncomeWSMessage(Json.parse(body).as[WSMessage])
        case _                        => RoomManager.UnsupportedWSMessage
      }

  private def source(
                      roomManager: ActorRef,
                      roomId: UUID,
                      userId: UUID,
                      name: String
                    ): Source[Message, ActorRef] =
    Source
      .actorRef[WSMessage](
        completionMatcher,
        failureMatcher,
        disabledBufferSize,
        OverflowStrategy.dropTail
      )
      // this is what is called when a user connects and has a session
      .mapMaterializedValue { userRef =>
        // send userjoin message to room manager
        roomManager ! RoomManager.ConnectToRoom(
          WSMessage(roomId, userId, UserJoin(name)),
          userRef
        )
        userRef
      }
      .map(message => TextMessage(Json.toJson(message).toString()))

  private val completionMatcher: PartialFunction[Any, CompletionStrategy] = {
    case RoomManager.CompleteWS => CompletionStrategy.immediately
  }

  private val failureMatcher: PartialFunction[Any, Throwable] = PartialFunction.empty

}

