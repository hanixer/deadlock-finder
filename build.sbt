name := "deadlock-finder"

version := "0.1"

scalaVersion := "3.1.0"

// https://mvnrepository.com/artifact/org.eclipse.jdt/org.eclipse.jdt.core
libraryDependencies += "org.eclipse.jdt" % "org.eclipse.jdt.core" % "3.27.0"

libraryDependencies += "org.typelevel" %% "paiges-core" % "0.4.2"

// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

// https://mvnrepository.com/artifact/org.jgrapht/jgrapht-core
libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.5.1"

Compile / run / mainClass := Some("deadlockFinder.Main")
