package gamedata

import gamedata.DeckOfCards.{Card, Deck, Heart, Rank, Spade, Three, Two}

import java.util.UUID
import akka.actor.{ActorRef => UntypedRef}
import gamedata.DeckOfCards.Deck.emptyDeck
import play.api.libs.json.{Format, Json}
// hand is optional because for some players that will be in a gamestate, their hands will be unknown from the perspective of other players
case class PlayerData(id: UUID, ref: Option[UntypedRef], name: String, hand: Option[Deck], points: Int, ranks: List[Rank], readied: Boolean) {
  def giveCard(card: Card): PlayerData = this.copy(hand = hand.map(x=>x.addToTop(card)))
  // for these functions the design choice is to take stlib collections where possible and return our custom types
  def giveCards(toGive: List[Card]): PlayerData ={ this.copy(hand = Option(Deck(toGive:::hand.get.toList)))}
  def takeCards(p : Card => Boolean): (Option[Deck], PlayerData) = {
    // todo handle the potential exception thrown by get better so this fn cannot throw exception
    hand match {
      case Some(hand) =>
        val (lost, kept) = hand.toList.partition(p)
        (Option.when(lost.nonEmpty)(Deck(lost)), this.copy(hand=Some(Deck(kept))))
      case None =>
        // user modeled doesn't have a hand
        (None, this)
    }
  }

  def makeGroups: (PlayerData, List[Rank]) = {
    // returns the player with their pairs converted to points and removed from their hands
    // and the Ranks they "paired" added to their ranks array
    // ._2 is the Ranks they paired
    hand match {
      case Some(hand)=>{
        val categorized: Map[Rank, List[Card]] = hand.toList.groupBy(x=>x.rank)
        val (remain, group) = categorized.values.partition(x=>x.length<4)
        val newhand = Deck(remain.flatten.toList)
        val newpoints = group.size
        val newranks = group.map(x=>x.head.rank).toList
        (this.copy(hand=Some(newhand), points = newpoints + points ,ranks=newranks++ranks), newranks)
      }
      case None=>(this, List())
    }
  }

  def hasCardWithRank(rank: Rank): Boolean = {
    // returns true if player has at least one card of the passed in rank in their hand
    hand.get.containsRank(rank)
  }
  def hasCard(card: Card): Boolean = {
    hand.get.containsCard(card)
  }
  def summary: String = {
    s"name: $name, hand (${hand.size}) cards: ${hand.map(x=>x.summary)}, pts: $points, ranks: $ranks"
  }
}
object PlayerData {
  val defaultPlayer = {
    PlayerData(UUID.randomUUID(), None, "player1", Some(emptyDeck), 0, List(), readied = false)
  }
  val defaultPlayerReadied = {
    PlayerData(UUID.randomUUID(), None, "player1", Some(emptyDeck), 0, List(), readied = true)
  }
  val defaultPlayerWCards = {
    defaultPlayerReadied.copy(hand = Some(Deck(List(Card(Two, Spade), Card(Three, Heart)))))
  }
}
