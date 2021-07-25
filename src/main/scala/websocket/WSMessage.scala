package websocket

import gamedata.DeckOfCards.{Card, Deck, Rank}
import gamedata.GoFish
import websocket.WSMessage.{
WSMessageType, WSMessageData
}
import play.api.libs.json._

import java.util.UUID
import scala.util.{Failure, Success, Try}
final case class WSMessage(messageType: WSMessageType, roomId: UUID, userId: UUID, payload: WSMessageData)
object WSMessage {
  sealed trait WSMessageData
  case class userJoin(name: String) extends WSMessageData
  case class pushState(gameState: GoFish) extends WSMessageData
  case class ask(askeeId: UUID, rank: Rank) extends WSMessageData

  val NoExtra = ""

  sealed trait WSMessageType {
    val stringRep: String
  }

  object WSMessageType {
    def apply(messageType: String): WSMessageType =
      messageType match {
        case Init.stringRep      => Init
        case Join.stringRep      => Join
        case Leave.stringRep     => Leave
        case Vote.stringRep      => Vote
        case _ => throw new IllegalArgumentException(s"$messageType is not a valid MessageType")
      }

    def unapply(messageType: WSMessageType): Option[String] =
      messageType match {
        case Init      => Option(Init.stringRep)
        case Join      => Option(Join.stringRep)
        case Leave     => Option(Leave.stringRep)
        case Vote      => Option(Vote.stringRep)

      }

    final case object Init extends WSMessageType {
      override val stringRep: String = "init"
    }

    final case object Join extends WSMessageType {
      override val stringRep: String = "join"
    }

    final case object Leave extends WSMessageType {
      override val stringRep: String = "leave"
    }

    final case object Vote extends WSMessageType {
      override val stringRep: String = "ask"
    }

    implicit val messageTypeFormat: Format[WSMessageType] = Format[WSMessageType](
      Reads[WSMessageType] {
        case JsString(value) =>
          Try(WSMessageType(value)) match {
            case Success(messageType) => JsSuccess(messageType)
            case Failure(exception)   => JsError(exception.toString)
          }
        case _ => JsError("Unexpected type")
      },
      Writes[WSMessageType] { messageType =>
        JsString(messageType.stringRep)
      }
    )
  }

  implicit val wsMessageFormat: Format[WSMessage] = Json.format[WSMessage]
}
