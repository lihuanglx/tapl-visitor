name := "tapl-visitor"

version := "1.0"

scalaVersion := "2.12.1"

scalacOptions ++= Seq("-language:higherKinds", "-language:implicitConversions", "-feature", "-deprecation")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.11"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"