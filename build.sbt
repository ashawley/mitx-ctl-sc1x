// build.sbt --- Scala build tool settings

scalaVersion := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8", "-Ywarn-adapted-args", "-Ywarn-dead-code", "-Ywarn-numeric-widen", "-Ywarn-inaccessible")

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze"      % "0.12",
  "org.specs2"   %% "specs2-core" % "3.7.2" % "test"
)
