name := "blockChainBeta"

version := "0.1"

scalaVersion := "2.12.7"

resolvers += "MavenRepository" at "https://oss.sonatype.org/content/repositories/releases"
resolvers += "MavenRepository" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies+="ch.qos.logback" % "logback-core" % "1.2.3"
libraryDependencies+="ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies+="org.mockito" % "mockito-core" % "2.21.0"
libraryDependencies+="com.github.pureconfig" %% "pureconfig" % "0.11.1"
libraryDependencies+="com.github.scopt" %% "scopt" % "3.7.1"
libraryDependencies+="org.scodec" %% "scodec-bits" % "1.1.6"
libraryDependencies+="io.monix" %% "monix" % "3.0.0"
libraryDependencies+="org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"


libraryDependencies+="org.scalatest" %% "scalatest" % "3.0.5"
libraryDependencies+="org.scalacheck" %% "scalacheck" % "1.14.0"
libraryDependencies+="ch.qos.logback" % "logback-core" % "1.2.3"
libraryDependencies+="ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies+="org.mockito" % "mockito-core" % "2.21.0"
libraryDependencies+="org.scodec" %% "scodec-bits" % "1.1.6"

libraryDependencies += "io.iohk" %% "decco" % "0.2"
libraryDependencies += "io.iohk" %% "decco-auto" % "0.2"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"


libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0"
libraryDependencies += "ch.qos.logback" % "logback-core" % "1.2.3"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "org.mockito" % "mockito-core" % "2.21.0"

libraryDependencies += "ch.qos.logback" % "logback-core" % "1.2.3"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "org.mockito" % "mockito-core" % "2.21.0"
libraryDependencies += "com.github.pureconfig" %% "pureconfig" % "0.11.1"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.1"
libraryDependencies += "org.scodec" %% "scodec-bits" % "1.1.6"
libraryDependencies += "io.monix" %% "monix" % "3.0.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0"
libraryDependencies += "ch.qos.logback" % "logback-core" % "1.2.3"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "org.mockito" % "mockito-core" % "2.21.0"
libraryDependencies += "org.scodec" %% "scodec-bits" % "1.1.6"


libraryDependencies+="io.monix" %% "monix" % "3.0.0"
libraryDependencies+="com.chuusai" %% "shapeless" % "2.3.3"
libraryDependencies+="org.slf4j" % "slf4j-api" % "1.7.25"
libraryDependencies+="io.netty" % "netty-all" % "4.1.31.Final"
libraryDependencies+="org.eclipse.californium" % "scandium" % "2.0.0-M15"
libraryDependencies+="org.eclipse.californium" % "element-connector" % "2.0.0-M15"
libraryDependencies+="org.scodec" %% "scodec-bits" % "1.1.6"
libraryDependencies+="io.iohk" %% "decco" % "0.2"
libraryDependencies+="io.iohk" %% "decco-auto" % "0.2"
libraryDependencies+="org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.26"


libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.19"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")