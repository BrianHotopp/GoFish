package gamedata

import actors.Room.CARDSPERPLAYER
import gamedata.DeckOfCards.Deck.{defaultDeck, emptyDeck}
import gamedata.DeckOfCards.{Card, Deck, Rank}
import gamedata.GoFish.GameStatus
import gamedata.PlayerData.{defaultPlayer, defaultPlayerReadied}
import play.api.libs.json.{Format, Json}

import java.util.UUID


// generic model of a gamestate
// deck is optional because from the perspective of the players, they don't know what the deck is
case class GoFish(players: List[PlayerData], deck: Option[Deck], turn: Option[UUID], gameStatus: GameStatus) {
  def playerHas(uuid: UUID, card: Card): Boolean ={
    players.find(x => x.id == uuid) match {
      case Some(player) => player.hasCard(card)
      case None => false
    }
  }
  def allPlayersReady(): Boolean = players.forall(x=>x.readied)
  def addPlayer(player: PlayerData): GoFish = {
    if(players.size == 0){
      // if this is the first player to be added to the game, set the turn
      this.copy(players = player::players, turn=Some(player.id))
    }else{
      this.copy(players = player::players)
    }

  }
  def removePlayerById(playerId: UUID): GoFish = {
    val (toRemove, toRemain) = this.players.partition(x=>x.id == playerId)
    val removalIndex = players.indexWhere(_.id == playerId)
    val turnIndex = players.indexWhere(_.id == turn.get)
    if(turnIndex > removalIndex){
      // if the player whose turn it is lies to the right of the player who is leaving, we must bump the turn up by one
      // mod toRemain.size should be irrelevant; it's just there to explicitly bound the index
      val newTurn = players((turnIndex + 1) % toRemain.size).id
      this.copy(players = toRemain, deck = Option(this.deck.get.addDeck(toRemove.head.hand.get)), turn = Some(newTurn)).shuffle
    }else{
      this.copy(players = toRemain, deck = Option(this.deck.get.addDeck(toRemove.head.hand.get))).shuffle
    }
  }

  def shuffle: GoFish = this.copy(deck=deck.map(x=>x.shuffle()))
  def dealToOne(numCards: Int, toDealTo: UUID): Option[GoFish] = {
    if(!deck.isDefined || numCards > deck.getOrElse(emptyDeck).size){
      None
    }else{
      val (toGive, newDeck) = deck.getOrElse(emptyDeck).toList.splitAt(numCards)
      val newPlayers = this.players.map(x=>if(x.id == toDealTo){x.giveCards(toGive)}else{x})
      Some(this.copy(players = newPlayers, deck = Some(Deck(newDeck))))
    }
  }
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
  def drawFromDeck(drawerId: UUID): (GoFish, Boolean) = {
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
            (this.copy(players = newPlayer::otherPlayers, deck=Some(newDeck)), needed)
          }
          case (None, _) => (this, false)
        }
      }
      case None => (this, false)
    }
  }
  def incrementTurn(): GoFish = {
    // returns the current game state but with the turn incremented
    val turnIndex = players.indexWhere(_.id == turn.get)
    val newTurn = players((turnIndex + 1) % players.size).id
    this.copy(turn=Some(newTurn))
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
          case (true, false) => {
            val (newGameState, neededCard) = this.drawFromDeck(asker.id)
            if(!neededCard){
              newGameState.incrementTurn()
            }else{
              newGameState
            }
          }
          case _ => {
            this
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
  val goFishStart = GoFish(List(), Some(defaultDeck), None, Waiting)
  val goFishNoRefsPreDeal = {
    val player1 = defaultPlayerReadied
    val player2 = defaultPlayerReadied.copy(name = "player2")
    val player3 = defaultPlayerReadied.copy(name = "player3")
    val player4 = defaultPlayerReadied.copy(name = "player4")
    GoFish(List(player1, player2, player3, player4), Some(defaultDeck), Some(player1.id), Waiting)
  }
  val goFishNoRefsPostDeal = {
    goFishNoRefsPreDeal.dealToAll(CARDSPERPLAYER).get
  }
  sealed trait GameStatus
  case object Waiting extends GameStatus
  case object Running extends GameStatus
  case object Finished extends GameStatus
}


