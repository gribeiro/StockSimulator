import aether.Aether._

import AssemblyKeys._

assemblySettings

name := "StockSimulator"

version := "1.3.11"

scalaVersion := "2.10.4"

publishMavenStyle := true

pomIncludeRepository := { x => true }

seq(aetherPublishSettings: _*)

resolvers += "Sonatype Nexus Manager Repository" at "http://apps.fiveware.com.br/nexus/content/repositories/releases/"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases"

resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "Element Releases" at "http://repo.element.hr/nexus/content/repositories/releases/"

resolvers += "akka" at "http://repo.akka.io/snapshots"

resolvers += "subcut" at "https://github.com/dickwall/subcut"

resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies += "org.spire-math" %% "spire" % "0.7.4"

libraryDependencies += "com.github.seratch" %% "awscala" % "0.2.+"

libraryDependencies += "commons-codec" % "commons-codec" % "1.2"

libraryDependencies += "commons-io" % "commons-io" % "2.4"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.0.3"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4-SNAPSHOT"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.4-SNAPSHOT"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.4"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3"

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "0.8.0"

libraryDependencies += "org.mongodb" %% "casbah" % "2.7.0"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.0" % "test"

libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.1.RC1" % "test"

libraryDependencies += "io.jvm" %% "scala-uuid" % "0.1.2"

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.31"	

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

javaOptions += "-XX:MaxPermSize=2048"

proguardSettings

ProguardKeys.options in Proguard ++= Seq("-dontnote", "-dontwarn", "-ignorewarnings")

ProguardKeys.options in Proguard += ProguardOptions.keepMain("com.stocksimulator.main.Bootstrap")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)

publishMavenStyle := true

publishTo := {
  val nexus = "http://apps.fiveware.com.br/nexus/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "content/repositories/releases")
}

credentials += Credentials("Sonatype Nexus Repository Manager", "apps.fiveware.com.br", "admin", "five2013")

mergeStrategy in assembly <<= (mergeStrategy in assembly) {
      (old) => {
        case PathList("org", "apache", xs @ _*) => MergeStrategy.last
        case str if str.endsWith("MMEwz.class") || str.endsWith("RatioArb2.class") || str.endsWith("RatioArb3.class") || str.endsWith("RatioArb.class") => MergeStrategy.discard
        case x => old(x)
      }
    }

publishArtifact in (Compile, packageDoc) := false

mainClass in Compile := Some("com.stocksimulator.main.Bootstrap")

mainClass in assembly := Some("com.stocksimulator.main.Bootstrap")

artifact in (Compile, assembly) ~= { art =>
  art.copy(`classifier` = Some("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)

assemblyOption in assembly ~= { _.copy(includeScala = true) }

assemblyOption in assembly ~= { _.copy(includeBin = true) }
