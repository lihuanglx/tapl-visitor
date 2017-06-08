scalaVersion in ThisBuild := "2.12.2"

lazy val localScalacOpts = Seq(
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-feature",
  "-deprecation")

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayRepo("scalameta", "maven"),

  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M9" cross CrossVersion.full),
  // addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),

  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions ++= localScalacOpts,
  scalacOptions in(Compile, console) := localScalacOpts
)

lazy val macros = project.settings(
  metaMacroSettings,
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0" //% Provided
)

lazy val tapl = project.settings(
  metaMacroSettings,
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
).dependsOn(macros)
