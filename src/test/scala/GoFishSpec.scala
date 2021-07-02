import DeckOfCards.{Card, Club, Deck, Diamond, Heart, King, Queen, Spade, Three, Two}
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
    val (playerOne, playerTwo) = (Player("PlayerOne").giveCards(Deck(List(Card(Two, Spade), Card(Two, Heart), Card(Three, Heart)))), Player("PlayerTwo"))
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
  it should "allocate points to each player based on their hand" in {
    // let's make a test goFish object
    val nPlayers = 2
    val (aPlayerId, anotherPlayerId) = (UUID.randomUUID(), UUID.randomUUID())
    val (twoHeart, twoSpade, twoClub, twoDiamond) = (Card(Two, Heart), Card(Two, Spade), Card(Two, Club), Card(Two, Diamond))
    val (threeHeart, threeSpade, threeClub, threeDiamond) = (Card(Three, Heart), Card(Three, Spade), Card(Three, Club), Card(Three, Diamond))
    val (queenHeart, queenDiamond, queenClub, queenSpade, kingHeart, kingDiamond) = (Card(Queen, Heart), Card(Queen, Diamond), Card(Queen, Club), Card(Queen, Spade), Card(King, Heart), Card(King, Diamond))
    val handWithOnePair = Deck(List(twoHeart, twoSpade, twoClub, twoDiamond, queenHeart))
    val handWithTwoPair = Deck(List(queenHeart, queenDiamond, queenClub, queenSpade, threeHeart, threeSpade, threeClub, threeDiamond, kingDiamond))
    val handWithNoPair = Deck(List(kingHeart))
    // players with hands with one and two pairs
    val (playerOne, playerTwo) = (Player("PlayerOne").giveCards(handWithOnePair), Player("PlayerTwo").giveCards(handWithTwoPair))
    // players with an empty hand and a non-empty hand with no pairs
    val (playerThree, playerFour) = (Player("PlayerThree"), Player("PlayerFour").giveCards(handWithNoPair))
    // check we have constructed the players correctly
    assert(playerOne.handSize == 5)
    assert(playerTwo.handSize == 9)
    assert(playerThree.handSize == 0)
    assert(playerFour.handSize == 1)
    // construct two gamestates each w 2 players
    val testGoFish1 = new GoFish(2, Map(aPlayerId->playerOne, anotherPlayerId->playerTwo), Deck())
    val testGoFish2 = new GoFish(2, Map(aPlayerId->playerThree, anotherPlayerId->playerFour), Deck())
    val res1 = testGoFish1.makeGroups
    val res2 = testGoFish2.makeGroups
    // check the players on res1
    val res1Players = res1._1.getPlayers
    val res2Players = res2._1.getPlayers
    res1Players.get(aPlayerId) match {
      case Some(player) => assert(!player.hasCard(twoSpade) && player.getScore == 1)
      case _ => assert(false)
    }
    res1Players.get(anotherPlayerId) match {
      case Some(player) =>
        assert(!player.hasCard(threeClub) && !player.hasCard(threeSpade) && player.hasCard(kingDiamond) && player.getScore == 2)
      case _ => assert(false)
    }
    res2Players.get(aPlayerId) match {
      case Some(player) => assert(player.getScore == 0)
      case _ => assert(false)
    }
    res2Players.get(anotherPlayerId) match {
      case Some(player) => assert(player.hasCard(kingHeart) && player.getScore == 0)
      case _ => assert(false)
    }
  }
}
