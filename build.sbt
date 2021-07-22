name := "gofish"

version := "0.1"

scalaVersion := "2.13.6"
val AkkaVersion = "2.6.15"
val AkkaHttpVersion = "10.2.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.9" % "test"
libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % AkkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.akka" %% "akka-stream-testkit" % AkkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % AkkaHttpVersion
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.2"
val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
