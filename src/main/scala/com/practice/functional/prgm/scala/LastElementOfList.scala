package com.practice.functional.prgm.scala

/**
  * Find the last element of a list.
  * Example:
  * scala> last(List(1, 1, 2, 3, 5, 8))
  * res0: Int = 8
  */

object LastElementOfList extends App {

  def lastElement[T](list: List[T]): T = {
    list match {
      case head :: Nil => head
      case _ :: tail => lastElement(tail)
      case _ => throw new Exception("Empty List")
    }
  }

  println(lastElement(List("A", "B", "C", "D", "E", "1")))

  println(lastElement(List(1, 1, 2, 3, 5, 8)))

  println(lastElement(List()))
}
