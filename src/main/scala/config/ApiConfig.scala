package config

import com.typesafe.config.Config

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

final case class ApiConfig(host: String, port: Int, timeout: FiniteDuration, indexPath: String)

object ApiConfig {
  def load(config: Config): ApiConfig = {
    ApiConfig(
      host = config.getString("go-fish.service.host"),
      port = config.getInt("go-fish.service.port"),
      timeout = FiniteDuration(
        config.getDuration("go-fish.service.timeout").toMillis,
        TimeUnit.MILLISECONDS
      ),
      indexPath = config.getString("go-fish.service.index-path")
    )
  }
}

