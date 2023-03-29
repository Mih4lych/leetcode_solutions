ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "test"
  )

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.7",
  "org.scalatest" %% "scalatest" % "3.2.7" % "test"
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

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)