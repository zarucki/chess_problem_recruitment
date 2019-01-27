name := "chess_challenge"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint", "-opt:l:inline", "-opt-inline-from:**")

javacOptions ++= Seq("-Xlint")

javaOptions in run ++= Seq("-Xms4g", "-Xmx8g")

fork in run := true

parallelExecution in Test := false
