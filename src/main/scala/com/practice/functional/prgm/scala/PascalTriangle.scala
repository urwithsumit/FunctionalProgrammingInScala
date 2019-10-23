package com.practice.functional.prgm.scala

import scala.annotation.tailrec

/**
  * Pascal Triangle example using Tail Recursion.
  */
object PascalTriangle extends App {

  @tailrec
  def pascalTriangle(limit: Int, level: Int, acc: Array[Array[Integer]]): Array[Array[Integer]] = {
    level match {
      case 0 => {
        acc(0)(0) = 1
        pascalTriangle(limit, level + 1, acc)
      }
      case _ if level == limit => acc
      case _ => {
        acc(level)(0) = 1
        acc(level)(level) = 1
        for (i <- 1 until level) {
          acc(level)(i) = acc(level - 1)(i - 1) + acc(level - 1)(i)
        }
        pascalTriangle(limit, level + 1, acc)
      }
    }
  }

  val limit = 7
  val Result = pascalTriangle(limit, 0, Array.ofDim(limit, limit))
  print(Result.map(_.filter(_ > 0).mkString(" ")).mkString("\n"))

}
