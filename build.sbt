name := """monsaintmoi"""
organization := "com.jmarzin"

version := "1.0"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.2"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.0" % Test
libraryDependencies += "org.mockito" % "mockito-core" % "1.8.5" % "test"


libraryDependencies += "com.typesafe.play" %% "play-slick" % "3.0.1"
libraryDependencies += "com.typesafe.play" %% "play-slick-evolutions" % "3.0.1"
libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1100-jdbc4"
// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.jmarzin.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.jmarzin.binders._"
