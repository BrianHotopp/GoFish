package websocket

import akka.actor.ActorRef
import gamedata.DeckOfCards.{Ace, Card, Club, Deck, Diamond, Eight, Five, Four, Heart, Jack, King, Nine, Queen, Rank, Seven, Six, Spade, Suit, Ten, Three, Two}
import gamedata.{GoFish, PlayerData}
import play.api.libs.json.{JsValue, _}

import java.util.UUID
import scala.util.{Failure, Success, Try}
/*
{
"roomId": "2d161581-1291-4a40-9565-2b855fe8348a",
"userId": "fa3eb75f-2ee5-4ba3-b772-e2e021c39f02",
"payload": {
"_type": "websocket.Ask",
"askeeId": "096b7720-c2b1-4d60-a452-795b46efec56",
"rank": "Two"
}
}
*/


final case class WSMessage(roomId: UUID, userId: UUID, payload: WSMessageData)
sealed trait WSMessageData
case class UserJoin(name: String) extends WSMessageData
case class PushState(gameState: GoFish) extends WSMessageData
case class Ask(askeeId: UUID, rank: Rank) extends WSMessageData

object WSMessage {
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
  implicit val suitFormat: Format[Suit] = Format[Suit](
    Reads[Suit] {
      case JsString(suitStr) => suitStr match {
        case "Club" => JsSuccess(Club)
        case "Spade" => JsSuccess(Spade)
        case "Heart" => JsSuccess(Heart)
        case "Diamond" => JsSuccess(Diamond)
        case _ => JsError("ERROR: Invalid Suit String!")
      }
      case _ => JsError("Unexpected type")
    },
    Writes[Suit] { Suit =>
      JsString(Suit.toString)
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
  implicit val cardFormat : Format[Card] = Json.format[Card]
  implicit val deckFormat : Format[Deck] = Json.format[Deck]
  implicit val playerDataFormat: Format[PlayerData] = Json.format[PlayerData]
  implicit val goFishFormat: Format[GoFish] = Json.format[GoFish]
  // wsmessage formats
  implicit val WSMessageData: Format[WSMessageData] = Json.format[WSMessageData]
  implicit val userJoinFormat: Format[UserJoin] = Json.format[UserJoin]
  implicit val pushStateFormat: Format[PushState] = Json.format[PushState]
  implicit val askFormat: Format[Ask] = Json.format[Ask]
  implicit val wsMessageFormat: Format[WSMessage] = Json.format[WSMessage]


}
