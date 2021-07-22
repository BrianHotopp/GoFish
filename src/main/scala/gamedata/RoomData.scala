package gamedata

import gamedata.DeckOfCards.Deck

import java.util.UUID


final case class RoomData(players: List[PlayerData], count: Int, deck: Deck){
  def addPlayer(playerToAdd: PlayerData): RoomData = {
    this.copy(players =playerToAdd :: players, count=count+1)
  }
  def deletePlayer(player: UUID): RoomData = {
    this.copy(players = players.filter(x=>x.id!=player), count=count-1)
  }
  def shuffle = this.copy(deck=deck.shuffle())
  def drawFromDeck(drawerId: UUID): Either[String, (RoomData, Boolean)] = {
    val (List(drawer), others) = players.partition(x=>x.id.equals(drawerId))
    val pullResult = deck.pullFromTop()
    pullResult match {
      case (Some(cardDrawn), remainingDeck) => {
        // resulting player from giving the drawer his drawn card
        val newPlayerData: PlayerData = drawer.giveCard(cardDrawn)
        val newDeck: Deck = remainingDeck
        val needed = drawer.hasCardWithRank(cardDrawn.rank)
        Right((this.copy(players = newPlayerData :: others, deck = newDeck), needed))
      }
      case (None, _) => Left("Can't draw; deck is empty!")
    }
  }
}
object RoomData {
  val initial: RoomData = RoomData(List(), 0, Deck())
}
