import DeckOfCards.{Card, Deck, Heart, Spade, Three, Two}
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
}

