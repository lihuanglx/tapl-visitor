name := "tapl-visitor"

version := "1.0"

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-language:higherKinds", "-language:implicitConversions", "-language:reflectiveCalls", "-feature", "-deprecation")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.12"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
