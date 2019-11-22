package com.scalaprogramming.fundamentals

/**
 * Find the number of elements of a ls.
 * Example:
 * scala> length(List(1, 1, 2, 3, 5, 8))
 * res0: Int = 6
 */
object NumOfElementInList extends App {

  def length[T](list: List[T]): Int = {

    def size(list: List[T], count: Int): Int = {
      list match {
        case Nil => count
        case _ :: tail => size(tail, count + 1)
      }
    }

    size(list, 0)
  }

  println(length(List(1, 1, 2, 3, 5, 8)))
  println(length(List()))
}
