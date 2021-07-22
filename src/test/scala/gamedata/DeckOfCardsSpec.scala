package gamedata

import gamedata.DeckOfCards._
import org.scalatest.FlatSpec
class DeckOfCardsSpec extends FlatSpec {
  "A deck of cards" should "report the right size" in {
    val testDeck = Deck()
    val testDeck2 = Deck(List())
    val testDeck3 = Deck(List(Card(Two, Heart)))
    assert(testDeck.size == 52)
    assert(testDeck2.size == 0)
    assert(testDeck3.size == 1)
  }
  it should "correctly report if it's empty" in {
    val testDeck = Deck(List())
    assert(testDeck.size == 0)
  }
  it should "correctly convert to Option" in {
    val testDeck = Deck()
    val testDeck2 = Deck(List())
    assert(!testDeck.toOption.isEmpty)
    assert(testDeck2.toOption.isEmpty)
  }
  it should "correctly convert to List" in {
   assert(Deck().toList.size == 52)
  }
  it should "shuffle cards correctly" in {
    val testDeck = Deck()
    val testDeck2 = Deck(testDeck).shuffle()
    assert(testDeck.toList.toSet == testDeck2.toList.toSet)
    assert(testDeck.toList != testDeck2.toList)
  }
  it should "get the correct card when pulling from the top" in {
    val ans = Deck().pullFromTop() match {
      case (Some(card), _) => card match {
        case Card(Two, Spade) => true
        case _ => false
      }
      case _ => false
    }
    assert(ans)
  }
  it should "remove the correct card from the deck when pulling from the top" in {
    val testDeck = Deck()
    val ans = testDeck.pullFromTop() match {
      case (Some(card), leftover) => {
        // original deck contained the card we got
        assert(testDeck.containsCard(card))
        // deck is 1 smaller and no longer contains the card we got
        assert(leftover.size == 51 && !leftover.containsCard(card))
        card match {
          case Card(Two, Spade) => true
          case _ => false
        }
      }
      case _ => false
    }
    // card is two of spades
    assert(ans)
  }
  it should "correctly add a single card to the top" in {
    val testDeck = Deck(List(Card(Two, Spade), Card(Three, Heart)))
    assert(!testDeck.containsCard(Card(Four, Club)) && testDeck.size == 2)
    val testDeck2 = testDeck.addToTop(Card(Four, Club))
    assert(testDeck2.containsCard(Card(Four, Club)) && testDeck2.size == 3)
  }
  it should "correctly add a list of cards to the top" in {
    val cardsList = Deck().toList.take(30)
    val testDeck = Deck(Deck().toList.drop(30))
    val finalDeck = testDeck.addToTop(cardsList)
    assert(
      finalDeck.size == 52 &&
        testDeck.size == 22 &&
        cardsList.size == 30 &&
        finalDeck.containsCard(Card(Two, Spade)) &&
        finalDeck.toList.head == Card(Two, Spade)
    )
  }
  it should "correctly report when it contains a certain rank" in {
    val testDeck = Deck()
    val testDeck2 = Deck(testDeck.toList.drop(4))
    assert(testDeck.containsRank(Two) && !testDeck2.containsRank(Two))
  }
  it should "correctly report when it contains a specific card" in {
    val testDeck = Deck()
    val testDeck2 = Deck(testDeck.toList.drop(4))
    val twoSpades = Card(Two, Spade)
    assert(testDeck.containsCard(twoSpades) && !testDeck2.containsCard(twoSpades))
  }
  it should "correctly allow adding a deck of cards to the top" in {
    val cardsList = Deck(Deck().toList.take(30))
    val testDeck = Deck(Deck().toList.drop(30))
    val finalDeck = testDeck.addDeck(cardsList)
    assert(
      finalDeck.size == 52 &&
      testDeck.size == 22 &&
      cardsList.size == 30 &&
      finalDeck.toList.head == Card(Two, Spade)
    )
  }
  it should "correctly allow filtering the deck based on some predicate" in {
    val testDeck = Deck()
    val resDeck = testDeck.filter(x => x == Card(Two, Spade))
    assert(resDeck.size == 1 && resDeck.toList.head == Card(Two, Spade))
  }
  it should "correctly allow partitioning the deck based on some predicate" in {
    val testDeck = Deck()
    val (resDeckL, resDeckR) = testDeck.partition(x => x.rank == Two)
    assert(resDeckL.size == 4 && resDeckR.size == 48 && resDeckL.containsRank(Two) && !resDeckR.containsRank(Two))
  }
  it should "correctly report if it contains the same set of cards as another deck" in {
    val testDeck = Deck().filter(x => x == Card(Two, Spade))
    val testDeck2 = Deck(List(Card(Two, Spade)))
    assert(testDeck.sameCards(testDeck2))
  }
}
