lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "pl.mikspec",
      scalaVersion := "2.13.1"
    )),
    name := "aoc2019"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
