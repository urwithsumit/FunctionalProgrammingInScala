package com.practice.functional.prgm.scala

import java.io.PrintWriter
import java.io.File

object ScalaFeaturesApp extends App {

  // Currying
  // Currying - Example 1:
  def addNo(x: Int)(y: Int)(z: Int) = x + y + z
  val onePlus = addNo(1) _
  println(onePlus(2)(3))
//
  // Currying - Example 2:
  def withPrintWriter(file: java.io.File)(op: PrintWriter => Unit) = {
    val Writer = new PrintWriter(file)
    try {
      op.apply(Writer)
    } finally {
      Writer.close()
    }
  }

  withPrintWriter(new java.io.File("date.txt")) { (x: PrintWriter) =>
    x.println(s"Date: ${new java.util.Date}")
  }


  // FoldLeft/Right
  val Money = List(1,1,1,1,1,1,1,1,1,1,1)

  val Total = Money.foldLeft(10)((acc, n) => acc + n )
  println(Total)

  val Names = Map("S" -> "K", "D" -> "K", "S" -> "A")

  val NameFoldLeftList = Names.foldLeft(List[String]())((acc, names) => s"""${names._1} ${names._2}""" :: acc)
  println(NameFoldLeftList)

  val NameFoldRightList = Names.foldRight(List[String]())((names, acc) => s""" ${names._1} ${names._2}""" :: acc)
  println(NameFoldRightList)

  println((2 to 10).foldLeft(1)(_ * _))
  println((2 to 10).reduceLeft(_ * _))

  println((2 to 10).foldRight(1)(_ * _))
  println((2 to 10).reduceRight(_ * _))

  val Family = Seq("A", "B", "C")
  val result = Family.zipWithIndex.map(c => (c._1,  c._2 + 1)).map(c => c.swap).mkString("\n")
  println(result)


  // Convert Methods to Partial Functions
  def Sum(x: Int, y: Int, z: Int) = x + y +z

  val a = Sum _

  val b = Sum(1, (_: Int), 8)

  println(s"a: ${a(1,2,3)}")
  println(s"b: ${b(2)}")

  val SomeNumber = List(1, 2,3,5)

  SomeNumber.foreach(println)
  SomeNumber.foreach(x => println(x))
  SomeNumber.foreach(println _)

  // Alternate Notation for Array[String]
  def arr(x: String*) = {
    x.foreach(println)
  }

  val Input = Array("S", "K")

  arr(Input: _*)  // This Tell Compiler to pass each element of Array as its own argument.


  // Chapter 9 - Odersky: Convert Method to Partial functions.
  def searchByFileCriteria(query: String, matcher: (String, String) => Boolean ) = {
    def getFiles = (new File("/Sumit/LeetCodes/src/main/scala/com/practice/leetcode").listFiles())
    for(file <- getFiles; if matcher(file.getName, query)) yield file.getName
  }

  println(searchByFileCriteria("scala", (fileName: String, query: String) => fileName.endsWith(query)).mkString(","))
  println(searchByFileCriteria("scala", _.endsWith(_)).mkString(","))


  def searchByFileCriteria(matcher: String => Boolean ) = {
    def getFiles = (new File("/Sumit/LeetCodes/src/main/scala/com/practice/leetcode").listFiles())
    for(file <- getFiles; if matcher(file.getName)) yield file.getName
  }

  println(searchByFileCriteria((fileName: String) => fileName.endsWith("scala")).mkString(","))
  println(searchByFileCriteria(_.endsWith("scala")).mkString(","))

}
