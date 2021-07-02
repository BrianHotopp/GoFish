import scala.io.StdIn.readLine
import DeckOfCards.Rank
object Main {
  def main(args: Array[String]): Unit ={
    // while no winner, take turns asking
    val nPlayers = 2
    var turn = 0
    var game = GoFish()
    val uuids = game.playerIds
    // deal and make pairs
    while(!game.gameOver){
      // allow player to ask for rank
      val playerName = game.getPlayerName(uuids(turn%nPlayers))
      println(s"$playerName's turn")
      println("Who would you like to ask?")
      (1 to nPlayers).foreach(n => {
        val name = game.getPlayerName(uuids(n-1)) match {
          case Some(name) => name
          case _ => "Unknown Player"
        }
        println(s"$n, ${name}")
      })
      val playerChoiceOption = readLine().toIntOption
      println("What would you like to ask for?")
      (1 to 13).foreach(n => println(s"$n "))
      val cardChoiceOption = readLine().toIntOption
      // ask/fish logic
      (playerChoiceOption, cardChoiceOption) match {
        case (Some(playerChoice), Some(cardChoice)) =>
          if(playerChoice > 0 && playerChoice <= nPlayers){
            val askerId = uuids(turn%nPlayers)
            val askeeId = uuids(playerChoice-1)
            // todo fix rank validation
            val wantedRank = Rank(cardChoice)
            game.askForCard(wantedRank, askerId, askeeId) match {
              case Right((newGameState, true)) =>
                println("Successfully stole!")
                game = newGameState
              case Right((_, false)) =>
                println("Go fish!")
                game.drawFromDeck(askerId) match {
                  case Right((stateAfterDraw, true)) =>
                    game = stateAfterDraw
                  case Right((stateAfterDraw, false)) =>
                    game = stateAfterDraw
                    turn+=1
                  case Left(err) => println(err)
                }
              case Left(err) => println(err)
            }
          }
        case (None, Some(_)) => println(s"Invalid person; please enter a number 1-$nPlayers")
        case (Some(_), None) => println("Invalid card rank; please enter a number 1-13")
        case (None, None) => println(s"Invalid person and invalid rank; please enter a number 1-$nPlayers and a number 1-13")
      }
      val paired = game.makeGroups
      game = paired._1
      // report grouping
      paired._2.foreachEntry((uuid, delta) => {
        val name = game.getPlayerName(uuid) match {
          case Some(name) => name
          case _ => "Unknown Player"
        }
        delta.foreach(rank=>println(s"${name})} made a pair with $rank"))
      }
      )
      }
  }
}
