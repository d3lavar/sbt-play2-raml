lazy val sbt_play2_raml = (project in file("."))
  .settings(
    name := "sbt-play2-raml",
    organization := "com.github.d3lavar",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.10.4",
    sbtVersion := "0.13.8",
    sbtPlugin := true,
    addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.3")
  )