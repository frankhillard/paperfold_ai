val scala3Version = "3.3.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "paperfold",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.2" % Test
  )
