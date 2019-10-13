package com.practice.functional.prgm.scala

import scala.annotation.tailrec

object DecimalToBinary extends App {

  // Convert Decimal to Binary
  @tailrec
  def convertToBinary(x: Int, acc: List[Int]= List()): List[Int] = {
    x match {
      case 0 if acc.isEmpty => 0 :: acc
      case 0 => acc
      case _ => convertToBinary(x / 2, (x % 2) :: acc)
    }
  }

  println(convertToBinary(183).mkString)

  println(convertToBinary(0).mkString)

  println(convertToBinary(1).mkString)
  println(convertToBinary(2).mkString)
  println(convertToBinary(3).mkString)
  println(convertToBinary(5).mkString)
  assert(convertToBinary(0).mkString == 0.toBinaryString)
  assert(convertToBinary(1).mkString == 1.toBinaryString)
  assert(convertToBinary(193).mkString == 193.toBinaryString)
  assert(convertToBinary(273).mkString == 273.toBinaryString)
  assert(convertToBinary(29999).mkString == 29999.toBinaryString)

}
