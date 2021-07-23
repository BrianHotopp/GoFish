package gamedata

import gamedata.DeckOfCards.{Card, Deck, Rank}

import java.util.UUID


// generic model of a gamestate
// deck is optional because from the perspective of the players, they don't know what the deck is
case class GoFish(players: List[PlayerData], deck: Option[Deck]) {
  def deckSize: Int = deck.size
  def gameOver: Boolean = players.map(x => x.getScore).sum == 13
  def playerHas(uuid: UUID, card: Card): Boolean ={
    players.find(x => x.id == uuid) match {
      case Some(player) => player.hasCard(card)
      case None => false
    }
  }
  def printPlayers(): Unit = {
    players.foreach(x=>println(x.summary))
  }
  def shuffle: GoFish = this.copy(deck=deck.map(x=>x.shuffle()))
  def dealToAll(numCards: Int): Option[GoFish] = {
    val nPlayers: Int = players.size
    val totalCards = numCards*nPlayers
    if(totalCards > deck.size){
      // can't deal more cards then exist
      None
    }else{
      // todo handle when there is no deck
      val (toDistribute, newDeck) = deck.get.toList.splitAt(totalCards)
      val newPlayers = players.zipWithIndex.map(x=>{
        x._1.copy(
          hand=Some(
            Deck(
              for{
                (card, index) <- toDistribute.zipWithIndex if index % nPlayers == x._2
              }yield{
                card
              }
            )
          )
        )
      })
      Some(this.copy(players = newPlayers, deck = Some(Deck(newDeck))))
    }
  }
  def drawFromDeck(drawerId: UUID): Either[String, (GoFish, Boolean)] = {
    players.find(x=>x.id == drawerId) match {
      case Some(drawer) => {
        // players who are not the drawer
        val otherPlayers = players.filter(x => x.id != drawerId)
        // resulting deck and card from draw action
        // todo fix the fact that this can throw
        val pullResult = deck.get.pullFromTop()

        pullResult match {
          case (Some(x), y) => {
            // resulting player from giving the drawer his drawn card
            val newPlayer: PlayerData = drawer.giveCard(x)
            val newDeck: Deck = y
            val needed = drawer.hasCardWithRank(x.rank)
            Right((this.copy(players = newPlayer::otherPlayers, deck=Some(newDeck)), needed))
          }
          case (None, _) => Left("Can't draw; deck is empty!")
        }
      }
      case None => Left("Invalid gamedata.Player!")
    }
  }

  def askForCard(wantedRank: Rank, askerId: UUID, askeeId: UUID): Either[String, (GoFish, Boolean)] = {
    (players.find(x=>x.id == askerId), players.find(x=>x.id == askeeId)) match {
      case (Some(asker), Some(askee)) => {
        (asker.hasCardWithRank(wantedRank), askee.hasCardWithRank(wantedRank)) match {
          case (true, true) => {
            val (Some(haul), newAskee) = {
              println(s"wanted rank: $wantedRank")
              askee.takeCards(x => x.rank == wantedRank)
            }
            val uninvolvedPlayers = players.filter(x => (x.id != askerId) && (x.id != askeeId))
            val newAsker = asker.giveCards(haul)
            val newGameState = this.copy(players = newAskee :: newAsker :: uninvolvedPlayers)
            Right((newGameState, true))
          }
          case (true, false) => Right((this, false))
          case (false, _) => Left("You must have at least one card of the rank you are asking for!")
        }
      }
      case (Some(_), None) => Left(s"Cannot find id ${askerId} to take cards from!")
      case (None, Some(_)) => Left(s"Cannot find id ${askeeId} to give cards to!")
      case (None, None) => Left(s"Cannot find asker id ${askerId} nor askee id ${askeeId}")
    }
  }
  def makeGroups: (GoFish, Map[UUID, List[Rank]]) = {
    // take groups of 4 from hand
    // construct new players with scores, ranks and hands newly created
    // construct delta group for info passing
    // idempotent
    // todo this can be done with one map
    val (newPlayers, delta) = players.map(x=>x.makeGroups).map(x=>(x._1, x._1.id -> x._2)).unzip
    // construct new gamedata.GoFish
    (this.copy(players = newPlayers), delta.toMap)
  }
}


