package com.scalaprogramming.fundamentals

/**
 * Reverse a ls.
 * Example:
 * scala> reverse(List(1, 1, 2, 3, 5, 8))
 * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
 */
object ReverseList extends App {

  def reverseList[T](list: List[T], acc: List[T] = List()): List[T] = {
    list match {
      case Nil => acc
      case head :: tail => reverseList(tail, head :: acc)
    }
  }

  println(reverseList(List(0,1,2,3,4,5)))
  println(reverseList(List(1, 1, 2, 3, 5, 8)))

}
