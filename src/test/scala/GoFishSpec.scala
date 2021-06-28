import DeckOfCards.{Card, Deck, Heart, Spade, Three, Two}
import org.scalatest.{EitherValues, FlatSpec, OptionValues}

import java.util.UUID
class GoFishSpec extends FlatSpec {
  "GoFish" should "report correct player name given a UUID" in {
    val testGoFish = GoFish()
    val tmp = testGoFish.getPlayerName(testGoFish.playerIds(0)) match {
      case Some(name) => name == "Brian"
      case None => false
    }
    assert(tmp)
  }
  it should "correctly allow a player to draw from the deck" in {
    val testGoFish = GoFish()
    val brianId = testGoFish.playerIds(0)
    // brian draws from the deck and didn't need it
    val x = testGoFish.drawFromDeck(brianId)
    var result = x match {
      case Right((_, false)) => true
      case _ => false
    }
    assert(result)
    // brian draws from the deck and needed it
    result = testGoFish.drawFromDeck(brianId) match{
      case Right((goFish, false)) => {
        goFish.drawFromDeck(brianId) match {
          case Right((resGoFish, true)) => true
          case _ => false
        }
      }
      case _ => false
    }
    // brian draws from the deck but can't because the deck is empty
  }
  it should "allow a player to take cards of a certain rank form another player" in {
    // let's make a test goFish object
    val nPlayers = 2
    val (playerOneId, playerTwoId) = (UUID.randomUUID(), UUID.randomUUID())
    val (playerOne, playerTwo) = (Player("PlayerOne").giveCards(Deck(List(Card(Two, Spade), Card(Two, Heart), Card(Three, Heart)))), Player("PLayerTwo"))
    val playerMap = Map(playerOneId -> playerOne, playerTwoId -> playerTwo)
    val deck = Deck()
    val testGoFish = new GoFish(nPlayers, playerMap, deck)
    val askres = testGoFish.askForCard(Two, playerTwoId, playerOneId)
    // playertwo successfully asks for player1's twos
    askres match {
      case Right((newGameState, true)) => {
        // check that player2 has the twos from playerone's hand
        // and playerone no longer has the two of spades or hearts
        assert(newGameState.playerHas(playerTwoId, Card(Two, Spade)) &&
          newGameState.playerHas(playerTwoId, Card(Two, Heart)) &&
          !newGameState.playerHas(playerOneId, Card(Two, Spade)) &&
          !newGameState.playerHas(playerOneId, Card(Two, Heart))
          )
      }
      case _ => false
    }
    // playertwo asks for player1's threes and misses

    val askres2 = testGoFish.askForCard(Two, playerTwoId, playerOneId)
    askres2 match {
      case Right((newGameState, false)) => {
        // check that the ask failed
        assert(!newGameState.playerHas(playerTwoId, Card(Two, Spade)) &&
          !newGameState.playerHas(playerTwoId, Card(Two, Heart)) &&
          newGameState.playerHas(playerOneId, Card(Two, Spade)) &&
          newGameState.playerHas(playerOneId, Card(Two, Heart))
        )
      }
      case _ => false
    }

  }
}
