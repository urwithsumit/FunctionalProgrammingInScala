name := "LeetCodes"

version := "0.1"

scalaVersion := "2.13.1"

(testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-u", "target/report")

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.13" % "1.2.0"