name := "StockSimulator"

git.baseVersion := "1.0"

versionWithGit

scalaVersion := "2.10.3"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"


resolvers += "Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases"

resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "Element Releases" at "http://repo.element.hr/nexus/content/repositories/releases/"

resolvers += "spray" at "http://repo.spray.io/"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

libraryDependencies +=
  "com.typesafe.akka" %% "akka-actor" % "2.3.0"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.10.0-M5"
)

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10+"

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "0.8.0"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.0.0"

libraryDependencies += "org.mongodb" % "casbah_2.10" % "2.7.0-RC0"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.0"

libraryDependencies += "org.jruby" % "jruby" % "1.7.10"

//libraryDependencies ++= List("org.scalamacros" % "quasiquotes" % "2.0.0-M3" cross CrossVersion.full)

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.0" % "test"

libraryDependencies +=
"org.scalamock" %% "scalamock-scalatest-support" % "3.0.1" % "test"

libraryDependencies += "io.jvm" %% "scala-uuid" % "0.1.2"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

javaOptions += "-XX:MaxPermSize=2048"

proguardSettings

ProguardKeys.options in Proguard ++= Seq("-dontnote", "-dontwarn", "-ignorewarnings")

ProguardKeys.options in Proguard += ProguardOptions.keepMain("com.stocksimulator.main.Bootstrap")

//addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)



