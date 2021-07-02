import DeckOfCards.{Card, Deck, Heart, Rank, Spade, Two}

import java.util.UUID



class GoFish(nPlayers: Int, players: Map[UUID, Player], deck: Deck) {
  // represents a game of go fish
  def getPlayerName(uuid: UUID): Option[String] = {
    players.get(uuid) match {
      case Some(player) => Some(player.getName)
      case _ => None
    }
  }
  def getPlayerScore(uuid: UUID): Option[Int] = {
    players.get(uuid).map(x=>x.getScore)
  }
  def getPlayerNames: List[String] = {
    players.values.map(x => x.getName).toList
  }
  def getPlayers: Map[UUID, Player] = players
  def playerIds: List[UUID] = {
    players.keys.toList
  }
  def deckSize = deck.size
  def gameOver: Boolean = players.values.map(x => x.getScore).sum == 13
  def playerHas(uuid: UUID, card: Card): Boolean ={
    players.get(uuid) match {
      case Some(player) => player.hasCard(card)
      case None => false
    }
  }

  def dealToAll(numCards: Int): Option[GoFish] = {
    val totalCards = numCards*nPlayers
    if(totalCards > deck.size){
      None
    }else{
      val (toDistribute, newDeck) = deck.toList.splitAt(totalCards)
      val newPlayers = players.zipWithIndex.map(x=>{
        x._1._1->
        Player(x._1._2, Deck(
          for{
            (card, index) <- toDistribute.zipWithIndex if index % nPlayers == x._2
          }yield{
           card
          }
        ))
      }).toMap
      Some(new GoFish(nPlayers, newPlayers, Deck(newDeck)))
    }
  }
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
            val uninvolvedPlayers = players.filter(x => (x._1 != askerId) && (x._1 != askeeId))
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
  def makeGroups: (GoFish, Map[UUID, List[Rank]]) = {
    // idempotent
    // how do I write this without triple mapping
    val groupResult = players.map(x=>x._1 -> x._2.makeGroups)
    val newPlayers = groupResult.map(x=> x._1->x._2._1)
    val delta = groupResult.map(x=>x._1->x._2._2)
    // construct new GoFish
    (new GoFish(nPlayers, newPlayers, deck), delta)
  }
}
object GoFish {
  // default constructor
  def apply(nPlayers: Int = 2, names: List[String] = List("Brian", "Kiara")): GoFish = {
    val uuids: List[UUID] = List.fill(nPlayers)(UUID.randomUUID())
    val pairs = for(
      n <- 0 to nPlayers-1
    ) yield(uuids(n), Player(names(n)))
    new GoFish(nPlayers, pairs.toMap, Deck())
  }
}


