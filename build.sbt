name := "Sudoku"

version := "0.1"

scalaVersion := "2.13.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")