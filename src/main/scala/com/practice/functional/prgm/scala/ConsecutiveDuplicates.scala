package com.practice.functional.prgm.scala

/**
 * Eliminate consecutive duplicates of list elements.
 * If a list contains repeated elements they should be replaced with a single
 * copy of the element.
 * The order of the elements should not be changed.
 * Example:
 *
 * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
 */

object ConsecutiveDuplicates extends App {

  def compress[T](list: List[T]): List[T] = {
    require(list.nonEmpty, "List is empty")

    def doCompression(list: List[T], acc: List[T]): List[T] = {
      list match {
        case Nil => acc
        case (head :: tail) if (head != acc.head) => doCompression(tail, head :: acc)
        case (_ :: tail) => doCompression(tail, acc)
      }
    }

    doCompression(list.tail, List(list.head)).reverse
  }

  println(compress(List("a", "a", "a", "a", "b", "c", "c", "c", "a", "a", "a", "d", "e", "e", "e")))
  println(compress(List()))
}
