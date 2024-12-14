ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode"
  )

libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2" % Test
