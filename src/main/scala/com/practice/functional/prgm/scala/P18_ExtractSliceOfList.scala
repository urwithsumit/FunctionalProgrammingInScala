package com.practice.functional.prgm.scala

/**
 * Extract a slice from a ls.
 * Given two indices, I and K, the slice is the ls containing the elements from and including the Ith element up to but not including the Kth element of the original ls. Start counting the elements with 0.
 * Example:
 *
 * scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res0: List[Symbol] = List('d, 'e, 'f, 'g)
 */

object P18_ExtractSliceOfList extends App {

  def sliceList[T](start: Int, end: Int, list: List[T]): List[T] = {
    list.zipWithIndex.filter(x => x._2 >= start && x._2 < end).map(y => y._1)
  }

  def sliceList2[T](start: Int, end: Int, list: List[T]): List[T] = {
    list.dropRight(list.size - end).drop(start)
  }

  println(sliceList(3, 7, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)))
  println(sliceList(3, 7, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")))

  println(sliceList2(3, 7, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)))
  println(sliceList2(3, 7, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")))

  println(sliceList2(3, 7, List(1, 2, 3, 4, 5, 6)))
  println(sliceList2(3, 7, List("a", "b", "c", "d")))
  println(sliceList2(3, 7, List()))

}
