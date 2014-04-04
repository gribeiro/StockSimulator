
resolvers += "jgit-repo" at "http://eclipse.ialto.org/jgit/maven/repository/"


addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.6.2")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")

addSbtPlugin("org.scala-sbt.plugins" % "sbt-onejar" % "0.8")

addSbtPlugin("com.orrsella" % "sbt-sublime" % "1.0.9")

addSbtPlugin("com.typesafe.sbt" % "sbt-proguard" % "0.2.2")

addSbtPlugin("no.arktekk.sbt" % "aether-deploy" % "0.11")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")