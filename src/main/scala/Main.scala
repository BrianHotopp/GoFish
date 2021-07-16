import scala.io.StdIn.readLine
import DeckOfCards.Rank
import actors.{Room, RoomManager}
import akka.NotUsed
import akka.actor.typed.scaladsl.AskPattern.{Askable, schedulerFromActorSystem}
import akka.actor.typed.{ActorRef, ActorSystem, Props, SpawnProtocol}
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives.{complete, get, path}
import akka.util.Timeout
import config.ApiConfig
import org.slf4j.{Logger, LoggerFactory}
import web.API

import java.util.UUID
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration.DurationInt
import scala.io.StdIn
import scala.language.implicitConversions
import scala.util.{Failure, Success}

object GoFishApp {


  val log: Logger = LoggerFactory.getLogger("GoFishAppMain")
  def main(args: Array[String]): Unit = {


    implicit val system =  ActorSystem(Behaviors.setup[SpawnProtocol.Command](_ => SpawnProtocol()), "go-fish")
    implicit val timeout: Timeout = 3.seconds

    val roomManagerFuture: Future[ActorRef[RoomManager.Command]] = system.ask { ref =>
      SpawnProtocol.Spawn(RoomManager(), "room-manager", Props.empty, ref)
    }
    implicit val ec: ExecutionContextExecutor = system.executionContext
    val apiConfig: ApiConfig = ApiConfig.load(system.settings.config)
    roomManagerFuture.onComplete {
      case Success(roomManager) =>
        val api = API(roomManager, apiConfig)
        api.run()
      case Failure(exception) =>
        log.error("Error creating room manager {}", exception)
    }
  }
}






















//object Main {
//
//  def main(args: Array[String]): Unit = {
//    // while no winner, take turns asking
//    val nPlayers = 2
//    var turn = 0
//    var game = GoFish()
//    //.shuffle
//    val uuids = game.playerIds
//    // deal and make pairs
//    game = game.dealToAll(4) match {
//      case Some(res) => res
//      case None => {
//        game
//      }
//    }
//    while (!game.gameOver) {
//      game.printPlayers
//      // allow player to ask for rank
//      val playerName = game.getPlayerName(uuids(turn % nPlayers)) match {
//        case Some(name) => name
//        case _ => "Unknown Player"
//      }
//      println(s"$playerName's turn")
//
//      if (game.getPlayers.get(uuids(turn % nPlayers)) match {
//        case Some(player) => {
//          player.handSize > 0
//        }
//        case None => false
//      }) {
//        println("Who would you like to ask?")
//        game.getPlayerNames.foreach(x =>
//          if (x != playerName) {
//            println(x)
//          } else {
//            println("Yourself")
//          })
//        //val aplayerChoiceOption = readLine().toIntOption
//        val playerChoiceOption = readLine().toIntOption
//        println("What would you like to ask for?")
//        (1 to 13).foreach(n => println(s"$n "))
//        val cardChoiceOption = readLine().toIntOption
//        // ask/fish logic
//        (playerChoiceOption, cardChoiceOption) match {
//          case (Some(playerChoice), Some(cardChoice)) =>
//            if (playerChoice > 0 && playerChoice <= nPlayers) {
//              val askerId = uuids(turn % nPlayers)
//              val askeeId = uuids(playerChoice - 1)
//              // todo fix rank validation
//              val wantedRank = Rank(cardChoice)
//              game.askForCard(wantedRank, askerId, askeeId) match {
//                case Right((newGameState, true)) =>
//                  println("Successfully stole!")
//                  val players = game.getPlayers
//                  game = newGameState
//                case Right((_, false)) =>
//                  println("Go fish!")
//                  game.drawFromDeck(askerId) match {
//                    case Right((stateAfterDraw, true)) =>
//                      game = stateAfterDraw
//                    case Right((stateAfterDraw, false)) =>
//                      game = stateAfterDraw
//                      turn += 1
//                    case Left(err) => println(err)
//                  }
//                case Left(err) => println(err)
//              }
//            }
//          case (None, Some(_)) => println(s"Invalid person; please enter a number 1-$nPlayers")
//          case (Some(_), None) => println("Invalid card rank; please enter a number 1-13")
//          case (None, None) => println(s"Invalid person and invalid rank; please enter a number 1-$nPlayers and a number 1-13")
//        }
//      } else {
//        println("No cards in hand! Going fishing!")
//        game.drawFromDeck(uuids(turn % nPlayers)) match {
//          case Right((stateAfterDraw, true)) =>
//            game = stateAfterDraw
//          case Right((stateAfterDraw, false)) =>
//            game = stateAfterDraw
//            turn += 1
//          case Left(err) => println(err)
//        }
//      }
//
//      val paired = game.makeGroups
//      game = paired._1
//      // report grouping
//      paired._2.foreachEntry((uuid, delta) => {
//        val name = game.getPlayerName(uuid) match {
//          case Some(name) => name
//          case _ => "Unknown Player"
//        }
//        delta.foreach(rank => println(s"${name})} made a pair with $rank"))
//      }
//      )
//    }
//  }
