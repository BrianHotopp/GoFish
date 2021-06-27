import scala.io.StdIn.readLine
import DeckOfCards.Rank
object Main {
  def main(args: Array[String]): Unit ={
    // while no winner, take turns asking
    val nPlayers = 2
    var turn = 0;
    var game = GoFish()
    val uuids = game.playerIds
    while(!game.gameOver){
      // allow player to ask for rank
      val playerName = game.getPlayerName(uuids(turn%nPlayers))
      println(s"${playerName}'s turn")
      println("Who would you like to ask?")
      (1 to nPlayers).foreach(n => println(s"${n}, ${game.getPlayerName(uuids(n-1))}"))
      val playerChoiceOption = readLine().toIntOption
      println("What would you like to ask for?")
      (1 to 13).foreach(n => println(s"${n} "))
      val cardChoiceOption = readLine().toIntOption

      (playerChoiceOption, cardChoiceOption) match {
        case (Some(playerChoice), Some(cardChoice)) => {
          val askerId = uuids(turn%nPlayers)
          val askeeId = uuids(playerChoice)
          // todo fix rank validation
          val wantedRank = Rank(cardChoice)
          game.askForCard(wantedRank, askerId, askeeId) match {
            case Right((newGameState, true)) => {
              println("Successfully stole!")
              game = newGameState
            }
            case Right((newGameState, false)) => {
              println("Go fish!")
              game = newGameState
              // todo go fishing in deck
            }
            case Left(err) => println(err)
          }
        }
        case (None, Some(_)) => println(s"Invalid person; please enter a number 1-${nPlayers}")
        case (Some(_), None) => println("Invalid card rank; please enter a number 1-13")
        case (None, None) => println(s"Invalid person and invalid rank; please enter a number 1-${nPlayers} and a number 1-13")
      }

      }
  }
}
