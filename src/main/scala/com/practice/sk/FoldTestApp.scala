package com.practice.sk

import java.io.File

import scala.annotation.tailrec

object FoldTestApp extends App {

  // FoldLeft/Right
  val Money = List(1,1,1,1,1,1,1,1,1,1,1)

  val Total = Money.foldLeft(10)((acc, n) => acc + n )
  println(Total)

  val Names = Map("Sumit" -> "Kumar", "Divit" -> "Kumar", "Shikha" -> "Agarwal")

  val NameFoldLeftList = Names.foldLeft(List[String]())((acc, names) => s"""${names._1} ${names._2}""" :: acc)
  println(NameFoldLeftList)

  val NameFoldRightList = Names.foldRight(List[String]())((names, acc) => s""" ${names._1} ${names._2}""" :: acc)
  println(NameFoldRightList)


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

  // Notation for Array[String]
  def arr(x: String*) = {
    x.foreach(println)
  }

  val Input = Array("Sumit", "kumar")

  arr(Input: _*)  // This Tell Compiler to pass each element of Array as its own argument.


  // Factorial
  @tailrec
  def factorial(x: Int, acc: BigInt): BigInt = {

    x match {
      case 0 => acc
      case 1 => acc
      case _ => factorial(x - 1, acc * x)
    }

  }

  println (factorial(10, 1))


  // Chapter 9 - Odersky.
  def searchByFileCriteria(query: String, matcher: (String, String) => Boolean ) = {
    def getFiles = (new File("/Sumit/LeetCodes/src/main/scala/com/practice/sk").listFiles())
    for(file <- getFiles; if matcher(file.getName, query)) yield file.getName
  }

  println(searchByFileCriteria("scala", (fileName: String, query: String) => fileName.endsWith(query)).mkString(","))
  println(searchByFileCriteria("scala", _.endsWith(_)).mkString(","))


  def searchByFileCriteria(matcher: String => Boolean ) = {
    def getFiles = (new File("/Sumit/LeetCodes/src/main/scala/com/practice/sk").listFiles())
    for(file <- getFiles; if matcher(file.getName)) yield file.getName
  }

  println(searchByFileCriteria((fileName: String) => fileName.endsWith("scala")).mkString(","))
  println(searchByFileCriteria(_.endsWith("scala")).mkString(","))


  
}
