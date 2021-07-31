package websocket

import akka.actor.ActorRef
import gamedata.DeckOfCards.{Ace, Card, Club, Deck, Diamond, Eight, Five, Four, Heart, Jack, King, Nine, Queen, Rank, Seven, Six, Spade, Suite, Ten, Three, Two}
import gamedata.{GoFish, PlayerData}
import websocket.WSMessage.{WSMessageData, WSMessageType}
import play.api.libs.json.{JsValue, _}

import java.util.UUID
import scala.util.{Failure, Success, Try}





final case class WSMessage(messageType: WSMessageType, roomId: UUID, userId: UUID, payload: WSMessageData)
object WSMessage {


  // formatters for json serde
  implicit val cardFormat : Format[Card] = Json.format[Card]
  implicit val mesageDataFormat: Format[WSMessageData] = Json.format[WSMessageData]
  implicit val userJoinFormat: Format[UserJoin] = Json.format[UserJoin]
  implicit val pushStateFormat: Format[PushState] = Json.format[PushState]
  implicit val askFormat: Format[Ask] = Json.format[Ask]

  // implicits from elsewhere
  implicit val playerDataFormat: Format[PlayerData] = Json.format[PlayerData]
  implicit val deckFormat : Format[Deck] = Json.format[Deck]
  implicit val goFishFormat: Format[GoFish] = Json.format[GoFish]
  implicit val rankFormat: Format[Rank] = Format[Rank](
    Reads[Rank] {
      case JsString(rankStr) => rankStr match {
        case "Two" => JsSuccess(Two)
        case "Three" => JsSuccess(Three)
        case "Four" => JsSuccess(Four)
        case "Five" => JsSuccess(Five)
        case "Six" => JsSuccess(Six)
        case "Seven" => JsSuccess(Seven)
        case "Eight" => JsSuccess(Eight)
        case "Nine" => JsSuccess(Nine)
        case "Ten" => JsSuccess(Ten)
        case "Jack" => JsSuccess(Jack)
        case "Queen" => JsSuccess(Queen)
        case "King" => JsSuccess(King)
        case "Ace" => JsSuccess(Ace)
        case _ => JsError("ERROR: Invalid Rank String!")
      }
      case _ => JsError("Unexpected type")
    },
    Writes[Rank] { rank =>
      JsString(rank.toString)
    }
  )
  implicit val suitFormat: Format[Suite] = Format[Suite](
    Reads[Suite] {
      case JsString(suiteStr) => suiteStr match {
        case "Club" => JsSuccess(Club)
        case "Spade" => JsSuccess(Spade)
        case "Heart" => JsSuccess(Heart)
        case "Diamond" => JsSuccess(Diamond)
        case _ => JsError("ERROR: Invalid suite String!")
      }
      case _ => JsError("Unexpected type")
    },
    Writes[Suite] { suite =>
      JsString(suite.toString)
    }
  )
  implicit val refFormat: Format[ActorRef] = Format[ActorRef](
    Reads[ActorRef] {
      case _ => JsSuccess(ActorRef.noSender)
    },
    Writes[ActorRef] { ref =>
      JsString(ref.toString)
    }
  )



  sealed trait WSMessageData
  case class UserJoin(name: String) extends WSMessageData
  case class PushState(gameState: GoFish) extends WSMessageData
  case class Ask(askeeId: UUID, rank: Rank) extends WSMessageData



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
        case PushState.stringRep => PushState
        case Ask.stringRep => Ask
        case Vote.stringRep      => Vote
        case _ => throw new IllegalArgumentException(s"$messageType is not a valid MessageType")
      }

    def unapply(messageType: WSMessageType): Option[String] =
      messageType match {
        case Init      => Option(Init.stringRep)
        case Join      => Option(Join.stringRep)
        case Leave     => Option(Leave.stringRep)
        case PushState => Option(PushState.stringRep)
        case Ask => Option(Ask.stringRep)
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
    final case object PushState extends WSMessageType {
      override val stringRep: String = "push"
    }

    final case object Ask extends WSMessageType {
      override val stringRep: String = "ask"
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
