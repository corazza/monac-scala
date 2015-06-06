name := "monac"

version := "0.0.1"

description := "Mona compiler in Scala"

organization := "org.monalang"

homepage := Some(url("http://www.monalang.org"))

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.6.1",
  "org.slf4j" % "slf4j-simple" % "1.6.1",
  "junit" % "junit" % "4.11" % "test",
  "org.mockito" % "mockito-all" % "1.8.4" % "test",
  "org.assertj" % "assertj-core" % "1.5.0" %  "test",
  "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
  )
