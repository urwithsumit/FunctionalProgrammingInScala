package com.practice.functional.prgm.scala

/**
  * P14 (*) Duplicate the elements of a ls.
  * Example:
  * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  *
  * P15 (**) Duplicate the elements of a ls a given number of times.
  * Example:
  * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  */
object P14_15_DuplicateElemOfList extends App {

  def duplicate[T](times: Int, list: List[T]): List[T] = {
    list.foldRight(List[T]())((c, r) =>
      List.fill(times)(c) ::: r
    )
  }

  //P14
  println(duplicate(2, List("a", "b", "c")))

  //P15
  println(duplicate(3, List("a", "b", "c")))
}
