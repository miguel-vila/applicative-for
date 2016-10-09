name := "applicative-for"

version := "0.0.0"

scalaVersion := "2.11.7"

lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % "2.11.7",
    "org.scalaz" %% "scalaz-core" % "7.1.0"
  )
)

lazy val appfor = project.in(file("appfor"))
  .settings(commonSettings: _*)
  .settings(name := "appfor")

lazy val client = project.in(file("client"))
  .settings(commonSettings: _*)
  .settings(name := "client")
  .dependsOn(appfor)


