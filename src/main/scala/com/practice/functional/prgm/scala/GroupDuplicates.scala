package com.practice.functional.prgm.scala

/**
 * Pack consecutive duplicates of list elements into sublists.
 * If a list contains repeated elements they should be placed in separate sublists.
 * Example:
 *
 * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
 */

object GroupDuplicates extends App {

  def groupingFoldLeft[T](list: List[T]): List[List[T]] = {
    val result = list.tail.foldLeft((List[List[T]](), List[T](list.head)))((r, c) => {
      if (r._2.contains(c))
        (r._1, c :: r._2)
      else
        (r._2 :: r._1, List(c))
    })

    (result._2 :: result._1).reverse
  }

  /**
   * In case of fold right, seed is left blank. This will add an empty list.
   * @param list
   * @tparam T
   * @return
   */
  def groupingFoldRight[T](list: List[T]): List[List[T]] = {
    val result = list.foldRight((List[List[T]](), List[T]()))((c, r) => {
      if (r._2.contains(c))
        (r._1, c :: r._2)
      else
        (r._2 :: r._1, List(c))
    })

    (result._2 :: result._1).filter(_.size > 0)
  }

  println(groupingFoldLeft(List("a", "a", "a", "a", "b", "c", "c", "c", "a", "a", "a", "d", "e", "e", "e")))
  println(groupingFoldRight(List("a", "a", "a", "a", "b", "c", "c", "c", "a", "a", "a", "d", "e", "e", "e")))

}
