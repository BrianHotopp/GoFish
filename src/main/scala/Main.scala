import scala.io.StdIn.readLine
object Main {
  def winner(players: Array[Player]): Boolean = {
    false
  }
  def main(args: Array[String]): Unit ={
    // while no winner, take turns asking
    val nplayers = 2
    var turn = 0;
    var game = GoFish(nplayers, List("Brian", "Kiara"))
    val uuids = game.playerIds()
    var decision = ""
    while(!game.gameOver()){
      // allow player to ask for rank
      val playerName = game.getPlayerName(uuids(turn%nplayers))
      println(s"${playerName}'s turn")
      println("Who would you like to ask?")
      (1 to nplayers).foreach(n => println(s"${n}, ${game.getPlayerName(uuids(n-1))}"))
      // todo validate this input
      val from = readLine()
      println("What would you like to ask for?")
      (1 to 13).foreach(n => println(s"${n} "))
      val cardChoice = readLine().toIntOption
      cardChoice match {
        case Some(validNumber) => {
          DeckOfCards.rankFromNumber(validNumber) match {
            case Some(rank) =>
              game.askForCard(rank, uuids(turn%nplayers), uuids(from.toInt)) match {
                case Right(potentialGameState) => potentialGameState match {
                  case Some(gamestate) => game = gamestate
                  case None => {
                    println("Go Fish!")
                    val drawres = game.drawFromDeck(uuids(turn%nplayers))
                    drawres match {
                      case (true, res) => {
                        res match {
                          case Right(gamestate) => game = gamestate
                          case Left(errstr) => println(errstr)
                        }
                      }
                      case (false, res) => {
                        res match {
                          case Right(gamestate) => {
                            turn = turn+1
                            game = gamestate
                          }
                          case Left(errstr) => println(errstr)
                        }
                      }

                    }
                  }
                }
                case Left(error) => println(error)
              }
            case None => println("Input number doesn't correspond to a rank!")
          }
        }
        case None => println("Invalid Number! You must choose a number between one (Ace) and 13 (King)")
      }
      }
  }
}
