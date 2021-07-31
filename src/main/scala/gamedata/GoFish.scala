package gamedata

import gamedata.DeckOfCards.Deck.{defaultDeck, emptyDeck}
import gamedata.DeckOfCards.{Card, Deck, Rank}
import play.api.libs.json.{Format, Json}

import java.util.UUID


// generic model of a gamestate
// deck is optional because from the perspective of the players, they don't know what the deck is
case class GoFish(players: List[PlayerData], deck: Option[Deck], turn: UUID) {
  def playerHas(uuid: UUID, card: Card): Boolean ={
    players.find(x => x.id == uuid) match {
      case Some(player) => player.hasCard(card)
      case None => false
    }
  }

  def addPlayer(player: PlayerData): GoFish = {
    if(players.size == 0){
      // if this is the first player to be added to the game, set the turn
      this.copy(players = player::players, turn=player.id)
    }else{
      this.copy(players = player::players)
    }

  }
  def removePlayerById(playerId: UUID): GoFish = {
    val (toRemove, toRemain) = this.players.partition(x=>x.id == playerId)
    val removalIndex = players.indexWhere(_.id == playerId)
    val turnIndex = players.indexWhere(_.id == turn)
    if(turnIndex > removalIndex){
      // if the player whose turn it is lies to the right of the player who is leaving, we must bump the turn up by one
      // mod toRemain.size should be irrelevant; it's just there to explicitly bound the index
      val newTurn = players((turnIndex + 1) % toRemain.size).id
      this.copy(players = toRemain, deck = Option(this.deck.get.addDeck(toRemove.head.hand.get)), turn = newTurn).shuffle
    }else{
      this.copy(players = toRemain, deck = Option(this.deck.get.addDeck(toRemove.head.hand.get))).shuffle
    }
  }

  def shuffle: GoFish = this.copy(deck=deck.map(x=>x.shuffle()))
  def dealToAll(numCards: Int): Option[GoFish] = {
    val nPlayers: Int = players.size
    val totalCards = numCards*nPlayers
    if(totalCards > deck.getOrElse(emptyDeck).size){
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
  def drawFromDeck(drawerId: UUID): GoFish = {
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
            this.copy(players = newPlayer::otherPlayers, deck=Some(newDeck))
          }
          case (None, _) => this
        }
      }
      case None => this
    }
  }
  def incrementTurn(): GoFish = {
    // returns the current game state but with the turn incremented
    val turnIndex = players.indexWhere(_.id == turn)
    val newTurn = players((turnIndex + 1) % players.size).id
    this.copy(turn=newTurn)
  }
  def askForCard(wantedRank: Rank, askerId: UUID, askeeId: UUID): GoFish = {
    (players.find(x=>x.id == askerId), players.find(x=>x.id == askeeId)) match {
      case (Some(asker), Some(askee)) => {
        (asker.hasCardWithRank(wantedRank), askee.hasCardWithRank(wantedRank)) match {
          case (true, true) => {
            val (haul, newAskee) = askee.takeCards(x => x.rank == wantedRank)
            val uninvolvedPlayers = players.filter(x => (x.id != askerId) && (x.id != askeeId))
            val newAsker = asker.giveCards(haul.getOrElse(emptyDeck).toList)
            val newGameState = this.copy(players = newAskee :: newAsker :: uninvolvedPlayers)
            newGameState
          }
          case _ => {
            // compute new turn
            this.incrementTurn()
          }
        }
      }
      case _ => this
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
  def mask(playerId: UUID): GoFish = {
    // return the gamestate with the things playerId shouldn't know masked out
    val newPlayers = this.players.map{
      player =>
      if(player.id != playerId){
        // inside this block is actions that should be performed for every player that is not the one we are masking for
        player.copy(ref=None, hand=None)
      }else{
        player.copy(ref=None)
      }
    }
    this.copy(players = newPlayers, deck = None)
  }

  def printPlayers(): Unit = {
    players.foreach(x=>println(x.summary))
  }
  def deckSize: Int = deck.size
  def gameOver: Boolean = players.map(x => x.points).sum == 13
}
object GoFish {
  val goFishNoRefsPreDeal = {
    val player1 = PlayerData(UUID.randomUUID(), None, "player1", Some(emptyDeck), 0, List())
    val player2 = PlayerData(UUID.randomUUID(), None, "player2", Some(emptyDeck), 0, List())
    val player3 = PlayerData(UUID.randomUUID(), None, "player3", Some(emptyDeck), 0, List())
    val player4 = PlayerData(UUID.randomUUID(), None, "player4", Some(emptyDeck), 0, List())
    GoFish(List(player1, player2, player3, player4), Some(defaultDeck), player1.id)
  }
  val goFishNoRefsPostDeal = {
    goFishNoRefsPreDeal.dealToAll(7).get
  }
}


