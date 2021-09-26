name := "scala-codecs"

version := "0.1"

scalaVersion := "2.13.5"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "ch.qos.logback"              % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.2",
  "co.fs2"                     %% "fs2-io"          % "3.0.3",
  "org.scodec"                 %% "scodec-stream"   % "3.0.2",
  "com.github.scopt"           %% "scopt"           % "4.0.0-RC2",
  "org.scalatest"              %% "scalatest"       % "3.2.2" % "test"
)

mainClass in (Compile, run) := Some("scalacodecs.ScalaCodecs")

mainClass in assembly := Some("scalacodecs.ScalaCodecs")
assemblyJarName in assembly := "scala-codecs.jar"
