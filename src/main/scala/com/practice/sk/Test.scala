package com.practice.sk

object Test extends App {

  println((2 to 10).foldLeft(1)(_ * _))
  println((2 to 10).reduceLeft(_ * _))

  println((2 to 10).foldRight(1)(_ * _))
  println((2 to 10).reduceRight(_ * _))

  val Family = Seq("Sumit", "Shikha", "Divit")
  val result = Family.zipWithIndex.map(c => (c._1,  c._2 + 1)).map(c => c.swap).mkString("\n")
  println(result)

}
