package gamedata

import gamedata.DeckOfCards.{Card, Deck, Rank}
import java.util.UUID
import akka.actor.{ActorRef => UntypedRef}
// hand is optional because for some players that will be in a gamestate, their hands will be unknown from the perspective of other players
case class PlayerData(id: UUID, ref: UntypedRef, name: String, hand: Option[Deck], points: Int, ranks: List[Rank]) {
  def giveCard(card: Card): PlayerData = {
    this.copy(hand = hand.map(x=>x.addToTop(card)))
  }
  def giveCards(toGive: Deck): PlayerData ={
    this.copy(hand=hand.map(x=>x.addDeck(toGive)))
  }
  def takeCards(p : Card => Boolean): (Option[Deck], PlayerData) = {
    // todo handle the potential exception thrown by get better so this fn cannot throw exception
    val (lost, kept) = hand.map(x=>x.partition(p)).get
    (lost.toOption, this.copy(hand=Some(kept)))
  }
  def summary = {
    s"name: $name, hand (${hand.size}) cards: ${hand.map(x=>x.summary)}, pts: $points, ranks: $ranks"
  }
  def makeGroups: (PlayerData, List[Rank]) = {
    // returns the player with their pairs converted to points and removed from their hands
    // and the Ranks they "paired" added to their ranks array
    // ._2 is the Ranks they paired
    val categorized: Map[Rank, List[Card]] = hand.get.toList.groupBy(x=>x.rank)
    val (remain, group) = categorized.values.partition(x=>x.length<4)
    val newhand = Deck(remain.flatten.toList)
    val newpoints = group.size
    val newranks = group.map(x=>x.head.rank).toList
    (this.copy(hand=Some(newhand), points = newpoints + points ,ranks=newranks++ranks), newranks)
  }
  def hasCardWithRank(rank: Rank): Boolean = {
    // returns true if player has at least one card of the passed in rank in their hand
    hand.get.containsRank(rank)
  }
  def hasCard(card: Card): Boolean = {
    hand.get.containsCard(card)
  }
  def getScore: Int = points

  def getName: String = name

  def getHand: Deck = hand.get

  def handSize: Int = hand.size

  def getRanks: List[Rank] = ranks
}
