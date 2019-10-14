name := "LeetCodes"

version := "0.1"

scalaVersion := "2.13.1"

(testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-u", "target/report")