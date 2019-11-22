package com.scalaprogramming.fundamentals

import scala.annotation.tailrec

object Factorial extends App {

  @tailrec
  def factorial(x: Int, acc: BigInt): BigInt = {

    x match {
      case 0 | 1 => acc
      case _ => factorial(x - 1, acc * x)
    }

  }

  println (factorial(10, 1))
}
