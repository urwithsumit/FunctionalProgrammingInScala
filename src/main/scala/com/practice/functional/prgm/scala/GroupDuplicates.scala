package com.practice.functional.prgm.scala

/**
 * P9: Pack consecutive duplicates of ls elements into sublists.
 * If a ls contains repeated elements they should be placed in separate sublists.
 * Example:
 *
 * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
 */

/**
 * P10 (*) Run-length encoding of a ls.
 * Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
 * Example:
 *
 * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
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
   * In case of fold right, seed is left blank. This will add an empty ls.
   *
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


  /**
   * P10 Solution
   * @param f
   * @tparam T
   * @return
   */
  def LengthEncodeOfList[T](f: => List[List[T]]): List[(Int, T)] = {
    f map (x => (x.size, x.head))
  }

  val Input = List("a", "a", "a", "a", "b", "c", "c", "c", "a", "a", "a", "d", "e", "e", "e")
  println(groupingFoldLeft(Input))
  println(groupingFoldRight(Input))
  println(LengthEncodeOfList(groupingFoldLeft(Input)))
  println(LengthEncodeOfList(groupingFoldRight(Input)))

}
