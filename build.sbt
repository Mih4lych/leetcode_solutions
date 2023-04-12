ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "test"
  )
val akkaVersion = "2.6.18"
val scalaTestVersion = "3.2.9"
val logbackVersion = "1.2.10"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.7",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)
libraryDependencies += "dev.zio" %% "zio"         % "2.0.0"
libraryDependencies += "dev.zio" %% "zio-streams" % "2.0.0"
libraryDependencies += "dev.zio" %% "zio-test"    % "2.0.0"
libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2" % Test
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.7.0",
)
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.2.0",
)
libraryDependencies += "co.fs2" %% "fs2-core" % "3.2.7"
libraryDependencies += "co.fs2" %% "fs2-io" % "3.2.7"

scalacOptions ++= Seq(
  "-language:higherKinds"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion,
)

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)