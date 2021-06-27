import DeckOfCards.Rank

import java.util.UUID



class GoFish(players: Map[UUID, Player], deck: DeckOfCards.Deck) {
  // represents a game of go fish
  val nplayers: Int = players.size
  def getPlayerName(uuid: UUID): String ={
    players.get(uuid) match {
      case Some(player) => player.getName
      case _ => "Unknown Player"
    }
  }
  def getPlayerNames: List[String] = {
    players.keys.map(x => getPlayerName(x)).toList
  }
  def playerIds(): List[UUID] = {
    players.keys.toList
  }
  def gameOver(): Boolean ={
    // returns true if all pairs of four are had, false otherwise
    val totalscore = players.values.map(x => x.getScore()).sum
    totalscore == 13
  }
  def drawFromDeck(drawer: UUID): (Boolean, Either[String, GoFish]) = {
    // returns a new game state with the player passed in having drawn a card, or an error string
    players.get(drawer) match {
      case Some(player) =>
        // players who are not the drawer
        val otherPlayers = players.filter(x => !x._1.equals(drawer))
        // resulting deck and card from draw action
        deck.pullFromTop() match {
          case (Some(x), y) =>
            // resulting player from giving the drawer his drawn card
            val newPlayer: Player = player.giveCard(x)
            val newDeck: DeckOfCards.Deck = y
            val needed = player.hasCardWithRank(x.rank)
            (needed, Right(new GoFish(otherPlayers + (drawer -> newPlayer), newDeck)))
          case (None, y) => (false, Left("Can't draw: No cards left in the deck!"))
        }
      case None => (true, Left("Invalid Player!"))
    }
  }

  def askForCard(rank: Rank, asker: UUID, from: UUID): Either[String, Option[GoFish]] ={
    players.get(asker) match {
      // check if the asker's uuid is a real player
      case Some(player) =>
        if (player.hasCardWithRank(rank)) {
          // check if the askee's uuid is a real player
          players.get(from) match {
            case Some(askee) =>
              if (askee.hasCardWithRank(rank)) {
                val res = askee.takeCards(x => x.rank == rank)

                // we got some cards
                val uninvolvedplayers = players.filter(x => x != asker && x != from)
                val newasker = player.giveCards(res._1)
                val newaskee = res._2

                Right(Some(new GoFish(uninvolvedplayers + (asker -> newasker) + (from -> newaskee), deck)))
              } else {
                Right(None)
              }
            case None => Left("Unknown askee uuid!")
          }
        } else {
          Left("You must have at least one card of the rank you are aking for!")
        }
      case None => Left("Unknown asker uuid!")
    }
  }
}
object GoFish {
  def apply(nplayers: Int, names: List[String]): GoFish = {
//    players: Map[UUID, Player] =
    val uuids: List[UUID] = List.fill(nplayers)(UUID.randomUUID())
    val pairs= for(
      uuid <- uuids;
      name <- names
    ) yield(uuid, Player(name))
    new GoFish(pairs.toMap, new DeckOfCards.Deck())
  }
}
