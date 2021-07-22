package gamedata

import gamedata.DeckOfCards._
import org.scalatest.FlatSpec
class PlayerSpec extends FlatSpec {
  "A player" should "correctly receive a card" in {
    val testPlayer = Player("Brian")
    assert(testPlayer.handSize == 0)
    val testPlayer2 = testPlayer.giveCard(Card(Two, Spade))
    assert(testPlayer2.handSize == 1 && testPlayer2.getName == "Brian" && testPlayer2.getScore == 0)
  }
  it should "correctly receive a deck of cards" in {
    val testPlayer = Player("Brian")
    val toGive = Deck(List(Card(Two, Spade), Card(Three, Heart)))
    val testPlayer2 = testPlayer.giveCards(toGive)
    assert(testPlayer2.handSize == 2 &&
    testPlayer2.getHand.containsCard(Card(Three, Heart)) &&
      testPlayer2.getName == "Brian" && testPlayer2.getScore == 0)
  }
  it should "correctly report if it has a card of a certain rank" in {
    val testPlayer = Player("Brian")
    assert(!testPlayer.hasCardWithRank(Two))
    val testPlayer2 = testPlayer.giveCards(Deck(List(Card(Two, Spade), Card(Three, Heart))))
    assert(testPlayer2.hasCardWithRank(Two))
  }
  it should "correctly convert pairs of cards in hand to points" in {
    val testPlayer1 = Player("Brian")
    val testPlayer2 = testPlayer1.giveCards(Deck(List(Card(Two, Spade), Card(Two, Heart), Card(Two, Club), Card(Two, Diamond))))
    val testPlayer3 = testPlayer2.giveCards(Deck(List(Card(Three, Spade), Card(Three, Heart), Card(Three, Club), Card(Three, Diamond))))
    val testPlayer4 = testPlayer3.giveCards(Deck(List(Card(Ace, Spade), Card(King, Heart), Card(Queen, Club), Card(Ace, Diamond))))
    val testResult1 = testPlayer1.makeGroups
    val testResult2 = testPlayer2.makeGroups
    val testResult3 = testPlayer3.makeGroups
    val testResult4 = testPlayer4.makeGroups

    assert(testPlayer4.handSize == 12 && !testPlayer4.getRanks.contains(Two))
    assert(testResult1._1.handSize == 0 && testResult1._1.getScore == 0 && !testResult1._1.getRanks.contains(Two))
    assert(testResult2._1.handSize == 0 && testResult2._1.getScore == 1 && testResult2._1.getRanks.contains(Two))
    assert(testResult3._1.handSize == 0 && testResult3._1.getScore == 2 && testResult3._1.getRanks.contains(Two) && testResult3._1.getRanks.contains(Three))
    assert(testResult4._1.handSize == 4 && testResult4._1.getScore == 2 && testResult4._1.getRanks.contains(Two) && testResult4._1.getRanks.contains(Three) && !testResult4._1.getRanks.contains(Queen))
  }
}

