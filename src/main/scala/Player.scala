import DeckOfCards.{Card, Deck, Rank}

class Player(name: String, hand: Deck, points: Int, ranks: List[Rank]) {
  def giveCard(card: Card): Player = {
    new Player(name, hand.addToTop(card), points, ranks)
  }
  def giveCards(toGive: Deck): Player ={
    new Player(name, hand.addDeck(toGive), points, ranks)
  }
  def takeCards(p : Card => Boolean): (Option[Deck], Player) = {
    val (lost, kept) = hand.partition(p)
    (lost.toOption, new Player(name, kept, points, ranks))

  }
  def makeGroups: (Player, List[Rank]) = {
    // returns the player with their pairs converted to points and removed from their hands
    // and the Ranks they "paired" added to their ranks array
    // ._2 is the Ranks they paired
    val categorized: Map[Rank, List[Card]] = hand.toList.groupBy(x=>x.rank)
    val (remain, group) = categorized.values.partition(x=>x.length<4)
    val newhand = Deck(remain.flatten.toList)
    val newpoints = group.size
    val newranks = group.map(x=>x.head.rank).toList
    (new Player(name, newhand, newpoints, newranks ++ ranks), newranks)
  }
  def hasCardWithRank(rank: Rank): Boolean = {
    // returns true if player has at least one card of the passed in rank in their hand
    hand.containsRank(rank)
  }
  def hasCard(card: Card): Boolean = {
    hand.containsCard(card)
  }
  def getScore: Int = points

  def getName: String = name

  def getHand: Deck = hand

  def handSize: Int = hand.size

  def getRanks: List[Rank] = ranks
}

object Player {
  // default constructor
  def apply(name: String): Player = {
   new Player(name, Deck(List()),0, List())
  }
  // copy constructor
  def apply(player: Player): Player = {
    new Player(player.getName, player.getHand, player.getScore, player.getRanks)
  }
  // copy but with new hand
  def apply(player: Player, newHand: Deck): Player = {
    new Player(player.getName, newHand, player.getScore, player.getRanks)
  }
}
