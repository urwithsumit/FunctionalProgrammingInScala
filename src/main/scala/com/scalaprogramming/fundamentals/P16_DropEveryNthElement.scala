package com.scalaprogramming.fundamentals

/**
  * P16 (**) Drop every Nth element from a ls.
  * Example:
  * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  */

object P16_DropEveryNthElement extends App {

  def dropNthElements[T](n: Int, list: List[T]): Seq[T] = {
    for (i <- 0 until list.size if (i + 1) % n != 0) yield list(i) // do i + 1, as its Zero index.
  }

  println(dropNthElements(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 11, 12, 13, 14)))

}
