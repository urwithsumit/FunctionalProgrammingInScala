package com.practice.functional.prgm.scala

import java.io.{File, PrintWriter}

/**
  * This is a mix of examples to practice the fold function implementations.
  *
  */
object ScalaFeaturesApp extends App {

  val onePlus = addNo(1) _
  // FoldLeft/Right
  val Money = List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  println(onePlus(2)(3))
  val Total = Money.foldLeft(10)((acc, n) => acc + n)

  withPrintWriter(new java.io.File("date.txt")) { (x: PrintWriter) =>
    x.println(s"Date: ${new java.util.Date}")
  }
  val Names = Map("S" -> "K", "D" -> "K", "S" -> "A")
  val NameFoldLeftList = Names.foldLeft(List[String]())((acc, names) => s"""${names._1} ${names._2}""" :: acc)
  println(Total)
  val NameFoldRightList = Names.foldRight(List[String]())((names, acc) => s""" ${names._1} ${names._2}""" :: acc)
  val Family = Seq("A", "B", "C")
  println(NameFoldLeftList)
  val result = Family.zipWithIndex.map(c => (c._1, c._2 + 1)).map(c => c.swap).mkString("\n")
  println(NameFoldRightList)

  println((2 to 10).foldLeft(1)(_ * _))
  println((2 to 10).reduceLeft(_ * _))

  println((2 to 10).foldRight(1)(_ * _))
  println((2 to 10).reduceRight(_ * _))
  val a = Sum _
  val b = Sum(1, (_: Int), 8)
  println(result)
  val SomeNumber = List(1, 2, 3, 5)
  val Input = Array("S", "K")

  // Currying
  // Currying - Example 1:
  def addNo(x: Int)(y: Int)(z: Int) = x + y + z

  println(s"a: ${a(1, 2, 3)}")
  println(s"b: ${b(2)}")

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

  SomeNumber.foreach(println)
  SomeNumber.foreach(x => println(x))
  SomeNumber.foreach(println _)

  // Convert Methods to Partial Functions
  def Sum(x: Int, y: Int, z: Int) = x + y + z

  // Alternate Notation for Array[String]
  def arr(x: String*) = {
    x.foreach(println)
  }

  arr(Input: _*) // This Tell Compiler to pass each element of Array as its own argument.

  // Chapter 9 - Odersky: Convert Method to Partial functions.
  def searchByFileCriteria(query: String, matcher: (String, String) => Boolean) = {
    def getFiles = (new File("/Sumit/LeetCodes/src/main/scala/com/practice/leetcode").listFiles())

    for (file <- getFiles; if matcher(file.getName, query)) yield file.getName
  }

  println(searchByFileCriteria("scala", (fileName: String, query: String) => fileName.endsWith(query)).mkString(","))
  println(searchByFileCriteria("scala", _.endsWith(_)).mkString(","))


  def searchByFileCriteria(matcher: String => Boolean) = {
    def getFiles = (new File("/Sumit/LeetCodes/src/main/scala/com/practice/leetcode").listFiles())

    for (file <- getFiles; if matcher(file.getName)) yield file.getName
  }

  println(searchByFileCriteria((fileName: String) => fileName.endsWith("scala")).mkString(","))
  println(searchByFileCriteria(_.endsWith("scala")).mkString(","))


  def ToString[T](list: List[T]): String = list match {
    case head :: tail => tail.foldLeft(head + "")((r, c) => r + " " + c)
    case Nil => ""
  }

  println(ToString(List(1, 2, 3, 4, 5, 6)))


  def reverse[T](list: List[T]): List[T] = {
    list.foldLeft(List[T]())((r, c) => c :: r)
  }

  println(reverse(List(1, 2, 3, 4, 5, 6)))


  def unique[T](list: List[T]): List[T] = {
    list.foldLeft(List[T]()) { (r, c) => if (!r.contains(c)) c :: r else r }.foldLeft(List[T]())((r, c) => c :: r)
  }


  println(unique(List(1, 1, 2, 2, 3, 4, 4, 5, 6, 6, 6, 6)))


  def ToSet[T](list: List[T]): Set[T] =
    list.foldLeft(Set[T]())((r, c) => r + c)


  println(ToSet(List(1, 1, 2, 2, 3, 4, 4, 5, 6, 6, 6, 6)))


  def insertionSort[A <% Ordered[A]](list: List[A]): List[A] = {
    list.foldLeft(List[A]()) { (r, c) =>

      val (front, back) = r.span(_ < c)
      front ::: c :: back
    }
  }

  println(insertionSort(List(4,2,5,44,6,1,1,4,56,8,6,88,6,5,99,3,2,1)))


  def myFunc[T](input: T): Option[T] =
    input match {
      case in: T => Some(in)
      case _ => None
    }


  println(myFunc(1))
  println(myFunc(""))
  println(myFunc("SK"))
  println(myFunc(null))
  println(myFunc(None))
  println(myFunc(1.0))
  println(myFunc(Nil))


  def testReduce() = {
    val list = List("foo", "Do", "bAr")

    println(list.withFilter(_.length == 2).map(str =>
    if(str == "foo")
      str.toUpperCase
    else
      str.toLowerCase
    ).reduce((l, r) => s"L: $l , R: $r"))
  }

  testReduce()
}
