val ScalaVer = "2.12.1"

lazy val commonSettings = Seq(
  name    := "shapeless-hmap"
, version := "0.0.1"
, scalaVersion := ScalaVer
, libraryDependencies ++= Seq(
    "org.typelevel"  %% "cats"      % "0.8.1"
  , "com.chuusai"    %% "shapeless" % "2.3.2"

  , "org.scalatest"  %% "scalatest" % "3.0.0"  % "test"
  )
, scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:experimental.macros",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      // "-Yinline-warnings",
      // "-Ywarn-dead-code",
      "-Xfuture")
)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(
    initialCommands := "import shapelesshmap._"
  )
