import DeckOfCards.{Rank, Deck}

import java.util.UUID



class GoFish(nPlayers: Int, players: Map[UUID, Player], deck: Deck) {
  // represents a game of go fish
  def getPlayerName(uuid: UUID): Option[String] = {
    players.get(uuid) match {
      case Some(player) => Some(player.getName)
      case _ => None
    }
  }

  def getPlayerNames: List[String] = {
    players.values.map(x => x.getName).toList
  }

  def playerIds: List[UUID] = {
    players.keys.toList
  }

  def gameOver: Boolean = players.values.map(x => x.getScore).sum == 13

  def drawFromDeck(drawerId: UUID): Either[String, (GoFish, Boolean)] = {
    players.get(drawerId) match {
      case Some(drawer) => {
        // players who are not the drawer
        val otherPlayers = players.filter(x => !x._1.equals(drawerId))
        // resulting deck and card from draw action
        val pullResult = deck.pullFromTop()

        pullResult match {
          case (Some(x), y) => {
            // resulting player from giving the drawer his drawn card
            val newPlayer: Player = drawer.giveCard(x)
            val newDeck: Deck = y
            val needed = drawer.hasCardWithRank(x.rank)
            Right((new GoFish(nPlayers, otherPlayers + (drawerId -> newPlayer), newDeck), needed))
          }
          case (None, _) => Left("Can't draw; deck is empty!")
        }
      }
      case None => Left("Invalid Player!")
    }
  }

  def askForCard(wantedRank: Rank, askerId: UUID, askeeId: UUID): Either[String, (GoFish, Boolean)] = {
    (players.get(askerId), players.get(askeeId)) match {
      case (Some(asker), Some(askee)) => {
        (asker.hasCardWithRank(wantedRank), askee.hasCardWithRank(wantedRank)) match {
          case (true, true) => {
            val (Some(haul), newAskee) = askee.takeCards(x => x.rank == wantedRank)
            val uninvolvedPlayers = players.filter(x => (x._1 != askerId) && (x._2 != askeeId))
            val newAsker = asker.giveCards(haul)
            val newGameState = new GoFish(nPlayers, uninvolvedPlayers + (askerId -> newAsker) + (askeeId -> newAskee), deck)
            Right((newGameState, true))
          }
          case (true, false) => Right((new GoFish(nPlayers, players, deck), false))
          case (false, _) => Left("You must have at least one card of the rank you are asking for!")
        }
      }
      case (Some(_), None) => Left(s"Cannot find id ${askerId} to take cards from!")
      case (None, Some(_)) => Left(s"Cannot find id ${askeeId} to give cards to!")
      case (None, None) => Left(s"Cannot find asker id ${askerId} nor askee id ${askeeId}")
    }
  }
}
object GoFish {
  // default constructor
  def apply(nPlayers: Int = 2, names: List[String] = List("Brian", "Kiara")): GoFish = {
    val uuids: List[UUID] = List.fill(nPlayers)(UUID.randomUUID())
    val pairs = for(
      uuid <- uuids;
      name <- names
    ) yield(uuid, Player(name))
    new GoFish(nPlayers, pairs.toMap, Deck())
  }

}

