package web
import actors.RoomManager
import actors.RoomManager.RoomId
import akka.actor.typed.{ActorRef, ActorSystem, SpawnProtocol}
import akka.actor.typed.SpawnProtocol.Spawn
import akka.actor.typed.scaladsl.AskPattern.{Askable, schedulerFromActorSystem}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter.TypedActorRefOps
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import config.ApiConfig
import org.slf4j.{Logger, LoggerFactory}
import websocket.WS

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.util.UUID
import scala.concurrent.Future
import scala.io.StdIn
import scala.util.{Failure, Success}
class API(roomManager: ActorRef[RoomManager.Command], apiConfig: ApiConfig)(implicit actorSystem: ActorSystem[SpawnProtocol.Command]
) {

  private implicit val timeout: Timeout = Timeout(apiConfig.timeout)
  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  val route: Route =
    concat(
      pathEndOrSingleSlash {
        get {
          log.debug("Index call")
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Hit the API Root</h1>"))
        }
      },
      path(JavaUUID) { roomId =>
        get {
          log.debug("Index call with room id: {}", roomId)
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Hit the API Root with UUID</h1>"))
        }
      },
      path("create-room") {
        post {
          log.debug("Create room call")
          onComplete((roomManager ? RoomManager.CreateRoom).mapTo[RoomId]) {
            case Success(result) => complete(result.value)
            case Failure(reason) =>
              log.error("Error while creating room: {}", reason)
              complete(StatusCodes.InternalServerError)
          }
        }
      },
      path("websocket" / JavaUUID / Remaining) { (roomId, encodedName) =>
        log.debug("Websocket call: {} {}", roomId, encodedName)
        handleWebSocketMessages(
          WS.handler(
            roomId,
            URLDecoder.decode(encodedName, StandardCharsets.UTF_8.name()),
            roomManager.toClassic
          )
        )
      }
    )
  def run(): Future[Http.ServerBinding] = {
    log.info("Starting API on host port {}:{}", apiConfig.host, apiConfig.port)
    Http().newServerAt(apiConfig.host, apiConfig.port).bind(route)
  }
}

object API {
  def apply(roomManager: ActorRef[RoomManager.Command], apiConfig: ApiConfig)(implicit actorSystem: ActorSystem[SpawnProtocol.Command]): API =
    new API(roomManager, apiConfig)
}



