package gamedata

import gamedata.DeckOfCards._
import gamedata.GoFish.goFishNoRefs
import org.scalatest.FlatSpec

import java.util.UUID
class GoFishSpec extends FlatSpec {

  it should "correctly allow a player to draw from the deck" in {
    val testGoFish = goFishNoRefs
    val player1Id = testGoFish.players(0).id
    // brian draws from the deck and didn't need it
    println(testGoFish.deck.get)
    val x = testGoFish.drawFromDeck(player1Id)

  }
  it should "allow a player to take cards of a certain rank from another player" in {}
  it should "allocate points to each player based on their hand" in {}
  it should "properly deal n cards to m players from the deck" in {}
}
