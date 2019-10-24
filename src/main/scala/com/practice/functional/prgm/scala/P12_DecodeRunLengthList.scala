package com.practice.functional.prgm.scala

/**
  * Decode a run-length encoded ls.
  * Given a run-length code ls generated as specified in problem P10, construct its uncompressed version.
  * Example:
  *
  * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  */

object P12_DecodeRunLengthList extends App {

  /**
    * Decode with Fold Left
    *
    * @param list
    * @tparam T
    * @return
    */
  def decode[T](list: List[(Int, T)]): List[T] = {
    list.foldLeft(List[T]())((r, c) =>
      List.fill(c._1)(c._2) ::: r
    )
  }.reverse

  /**
    * Decode with Fold Right.
    *
    * @param list
    * @tparam T
    * @return
    */
  def decode2[T](list: List[(Int, T)]): List[T] = {
    list.foldRight(List[T]())((c, r) =>
      List.fill(c._1)(c._2) ::: r
    )
  }

  println(decode(List((4, "a"), (1, "b"), (3, "c"), (3, "a"), (1, "d"), (3, "e"))))

  println(decode2(List((4, "a"), (1, "b"), (3, "c"), (3, "a"), (1, "d"), (3, "e"))))
}
