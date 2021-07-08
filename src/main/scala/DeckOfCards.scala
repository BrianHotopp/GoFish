import scala.math.ScalaNumericConversions
import scala.util.Random

object DeckOfCards {

  //clumsy enumeration definition
  sealed abstract class Suite
  case object Spade extends Suite
  case object Heart extends Suite
  case object Club extends Suite
  case object Diamond extends Suite

  sealed abstract class Rank
  object Rank {
    def apply(number: Int): Rank = {
      if(number < 1 || number > 13){
        return Ace
      }else{
        return ranks(number-2)
      }
    }
  }
  case object Two extends Rank
  case object Three extends Rank
  case object Four extends Rank
  case object Five extends Rank
  case object Six extends Rank
  case object Seven extends Rank
  case object Eight extends Rank
  case object Nine extends Rank
  case object Ten extends Rank
  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank
  case object Ace extends Rank

  val suites = Set(Spade, Heart, Club, Diamond)
  val ranks = List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)

  //the interesting part
  case class Card(rank: Rank, suite: Suite)

  class Deck(pCards: List[Card] = for (r <- ranks; s <- suites) yield Card(r, s)) {

    val cards: List[Card] = if (isValidDeck(pCards)) pCards
    else throw new RuntimeException("Deck is invalid!")

    def size = cards.size

    def isEmpty = cards.size == 0

    def toOption = if(isEmpty){None}else{Some(Deck(cards))}

    def toList = cards

    def shuffle() = new Deck(Random.shuffle(cards))

    def pullFromTop() = (cards.headOption, new Deck(cards.drop(1)))
    def summary = {
      s"$cards"
    }
    def addToTop(card: Card) = new Deck(card :: cards)

    def addToTop(cardsToAdd: List[Card]) = new Deck(cardsToAdd ::: cards)

    def containsRank(searchRank: Rank): Boolean ={
      cards.map((card) => card.rank).contains(searchRank)
    }
    def containsCard(card: Card): Boolean = {
      cards.contains(card)
    }
    def addDeck(deck: Deck): Deck = {
      val testDeck = Deck(cards)
      testDeck.addToTop(deck.toList)
    }
    def filter(f: Card => Boolean): Deck = {
      Deck(cards.filter(f))
    }
    def partition(f: Card => Boolean): (Deck, Deck) = {
      val tmp = cards.partition(f)
      (Deck(tmp._1), Deck(tmp._2))
    }
    def sameCards(other: Deck): Boolean = {
      other.toList.toSet == cards.toSet
    }
    private def isValidDeck(cards: List[Card]) = true
     // cards.size <= 5 && cards.distinct.size == cards.size

  }
  object Deck {
    // copy constructor
    def apply(deck: Deck): Deck = {
      new Deck(deck.cards)
    }
    // constructor from list
    def apply(base: List[Card]): Deck ={
      new Deck(base)
    }
    // empty constructor
    def apply(): Deck = {
      new Deck()
    }
  }

}