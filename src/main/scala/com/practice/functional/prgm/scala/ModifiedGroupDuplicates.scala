package com.practice.functional.prgm.scala

/**
 * P11 (*) Modified run-length encoding.
 * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
 * Example:
 *
 * scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
 *
 */
object ModifiedGroupDuplicates extends App {

  def modifiedGroupFoldLeft[T](list: List[T]): List[Any] = {
    val Result = list.tail.foldLeft(List[Any](), List[T](list.head))((r, c) => {
      r._2.head match {
        case x if x == c => (r._1, c :: r._2)
        case x if x != c && r._2.size == 1 => (x :: r._1, List(c))
        case _ => (r._2 :: r._1, List(c))
      }
    })

    (Result._2 :: Result._1).reverse
  }

  def compress[T](f: => List[Any]): List[Any] = {
    f.map(x => x match {
      case y: List[T] => (y.size, y.head)
      case _ => x
    })
  }

  val Input = List("a", "a", "a", "a", "b", "c", "c", "c", "a", "a", "a", "d", "e", "e", "e")
  println(modifiedGroupFoldLeft(Input))
  println(compress(modifiedGroupFoldLeft(Input)))
}
