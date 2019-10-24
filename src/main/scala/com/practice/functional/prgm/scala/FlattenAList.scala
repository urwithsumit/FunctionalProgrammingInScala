package com.practice.functional.prgm.scala

/**
 * Flatten a nested ls structure.
 * Example:
 * scala> flattenColl(List(List(1, 1), 2, List(3, List(5, 8))))
 * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
 */

object FlattenAList extends App {

  def flattenColl(list: List[Any]): List[Any] = {
    list.iterator.flatMap {
      case sublist: List[Any] => flattenColl(sublist) // If type is a ls, flattenColl it further using the recursive call to itself
      case data => List(data) // add to a ls, flatMap will eventually all of these elements together in a single ls.
    }.toList
  }

  println(flattenColl(List(List(1, 1), 2, List(3, List(5, 8)))))

  println(flattenColl(List(List("S", 1), "$", List(3, List(5, "!")))))
}
