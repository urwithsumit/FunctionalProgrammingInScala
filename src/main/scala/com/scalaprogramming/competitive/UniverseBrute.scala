package com.scalaprogramming.competitive

object UniverseBrute extends App {

  import scala.io.StdIn._

  var input = -1
  val acc = scala.collection.mutable.ArrayBuffer[Int]()
  while(input != 42) {
    input = readInt()
    if(input != 42) acc += input
  }

  println(acc.mkString("\n"))

}
