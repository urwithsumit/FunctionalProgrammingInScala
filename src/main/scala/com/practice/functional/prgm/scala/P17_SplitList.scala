package com.practice.functional.prgm.scala

object P17_SplitList extends App {

  def split[T](n: Int, list: List[T]): (List[T], List[T]) = {

    val counter = 0
    val Result = list.foldLeft((List[T](), List[T](), counter))((r, c) =>
      if (r._3 < n) {
        (c :: r._1, r._2, r._3 + 1)
      } else {
        (r._1, c :: r._2, r._3)
      }
    )

    (Result._1.reverse, Result._2.reverse)
  }

  def split2[T](n: Int, ls: List[T]): (List[T], List[T]) = {
    (ls.take(n), ls.drop(n))
  }

  println(split(3, List(1, 2, 3, 4, 5, 6)))

  println(split2(3, List(1, 2, 3, 4, 5, 6, 6, 5, 4, 3, 2, 2, 1)))


}
