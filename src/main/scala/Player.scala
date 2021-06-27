import DeckOfCards.{Card, Deck, Rank}

class Player(name: String, hand: Deck, points: Int) {
  def giveCard(card: Card): Player = {
    new Player(name, hand.addToTop(card), points)
  }
  def giveCards(toGive: Deck): Player ={
    new Player(name, hand.addDeck(toGive), points)
  }
  def takeCards(p : Card => Boolean): (Option[Deck], Player) = {
    val (lost, kept) = hand.partition(p)
    (lost.toOption, new Player(name, kept, points))

  }
  def hasCardWithRank(rank: Rank): Boolean = {
    // returns true if player has at least one card of the passed in rank in their hand
    hand.containsRank(rank)
  }
  def getScore = points

  def getName = name

  def getHand = hand
}

object Player {
  // default constructor
  def apply(name: String): Player = {
   new Player(name, Deck(),0)
  }
  // copy constructor
  def apply(player: Player): Player = {
    new Player(player.getName, player.getHand, player.getScore)
  }
  // copy but with new hand
  def apply(player: Player, newHand: Deck): Player = {
    new Player(player.getName, newHand, player.getScore)
  }
}
