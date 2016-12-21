name := "paralell"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies +=
  "com.typesafe.akka" %% "akka-actor" % "2.5-SNAPSHOT"


libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.7"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
