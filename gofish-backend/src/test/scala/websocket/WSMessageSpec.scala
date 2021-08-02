package websocket

import akka.http.scaladsl.model.ws.TextMessage
import gamedata.DeckOfCards.{Card, Heart, Two}
import org.scalatest.FlatSpec
import play.api.libs.json.Json
import WSMessage._
import gamedata.DeckOfCards.Deck.defaultDeck
import gamedata.GoFish.goFishNoRefsPostDeal
import gamedata.PlayerData.defaultPlayerWCards

import java.util.UUID
class WSMessageSpec extends FlatSpec {
  "A WSMessage" should "correctly convert a card to json" in {
    val res = Json.toJson(Card(Two, Heart)).toString()
    val cmp = """{"rank":"Two","Suit":"Heart"}"""
    assert(res.equals(cmp))
  }
  it should "correctly convert a Deck to json" in {
    val res = Json.toJson(defaultDeck).toString()
  }
  it should "correctly convert PlayerData to json" in {
    val res = Json.toJson(defaultPlayerWCards).toString()
    println(res)
  }
  it should "correctly convert GoFish to json" in {
    val res = Json.toJson(goFishNoRefsPostDeal)
    println(res)
  }
  it should "correctly parse incoming Ask message as json" in {
    val roomId = UUID.randomUUID()
    val userId = UUID.randomUUID()
    val askeeId = UUID.randomUUID()
    val toRec = WSMessage(roomId, userId, Ask(askeeId, Two))
    println(Json.toJson(toRec))
  }

}
