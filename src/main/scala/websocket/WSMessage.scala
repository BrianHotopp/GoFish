package websocket

import gamedata.DeckOfCards.{Card, Deck, Rank}
import websocket.WSMessage.MessageType
import play.api.libs.json._

import java.util.UUID
import scala.util.{Failure, Success, Try}

final case class WSMessage(messageType: MessageType, roomId: UUID, userId: UUID, data: Map[String, String])

object WSMessage {

  val NoExtra = ""

  sealed trait MessageType {
    val stringRep: String
  }

  object MessageType {
    def apply(messageType: String): MessageType =
      messageType match {
        case Init.stringRep      => Init
        case Join.stringRep      => Join
        case Leave.stringRep     => Leave
        case Vote.stringRep      => Vote
        case _ => throw new IllegalArgumentException(s"$messageType is not a valid MessageType")
      }

    def unapply(messageType: MessageType): Option[String] =
      messageType match {
        case Init      => Option(Init.stringRep)
        case Join      => Option(Join.stringRep)
        case Leave     => Option(Leave.stringRep)
        case Vote      => Option(Vote.stringRep)

      }

    final case object Init extends MessageType {
      override val stringRep: String = "init"
    }

    final case object Join extends MessageType {
      override val stringRep: String = "join"
    }

    final case object Leave extends MessageType {
      override val stringRep: String = "leave"
    }

    final case object Vote extends MessageType {
      override val stringRep: String = "ask"
    }

    implicit val messageTypeFormat: Format[MessageType] = Format[MessageType](
      Reads[MessageType] {
        case JsString(value) =>
          Try(MessageType(value)) match {
            case Success(messageType) => JsSuccess(messageType)
            case Failure(exception)   => JsError(exception.toString)
          }
        case _ => JsError("Unexpected type")
      },
      Writes[MessageType] { messageType =>
        JsString(messageType.stringRep)
      }
    )
  }

  implicit val wsMessageFormat: Format[WSMessage] = Json.format[WSMessage]
}
