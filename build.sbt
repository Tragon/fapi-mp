organization := "scala"

name := "FAPI"

version := "1.0"

scalaVersion := "2.13.1"

//javacOptions ++= Seq("-source", "1.8")

scalacOptions += "-feature"

javaOptions += "-Xmx2048m"

javaOptions += "-XX:MaxPermSize=1024m"

//javaHome := Some(file("C:/Program Files/Java/jdk1.8.0_161"))
//javaHome := Some(file("C:\\Users\\Tragon\\.jdks\\corretto-17.0.7"))

// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % Test

// https://mvnrepository.com/artifact/com.typesafe/config
libraryDependencies += "com.typesafe" % "config" % "1.4.0"

// https://mvnrepository.com/artifact/com.typesafe.akka/akka-actor
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.6.3"

// https://mvnrepository.com/artifact/org.jsoup/jsoup
libraryDependencies += "org.jsoup" % "jsoup" % "1.13.1"

// https://mvnrepository.com/artifact/org.joda/joda-convert
libraryDependencies += "org.joda" % "joda-convert" % "2.2.1"

// https://mvnrepository.com/artifact/joda-time/joda-time
libraryDependencies += "joda-time" % "joda-time" % "2.10.5"

// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-parser-combinators
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

parallelExecution in Test := false
