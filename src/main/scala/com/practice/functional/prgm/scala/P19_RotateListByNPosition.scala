package com.practice.functional.prgm.scala

/**
 * Rotate a ls N places to the left.
 * Examples:
 * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
 *
 * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
 */

object P19_RotateListByNPosition extends App {

  /**
   * Starts rotation from right side of ls e.e last element is put on the current of ls
   *
   * @param n
   * @param list
   * @tparam T
   * @return
   */
  def rotateRight[T](n: Int, list: List[T]): List[T] = rotateLeft(n, list.reverse)

  def rotateLeft[T](n: Int, ls: List[T]): List[T] = {
    def doRotation[T](ls: List[T], count: Int): List[T] = {
      ls.tail.foldLeft(List[T](ls.last), ls.head)((r, c) => {
        (r._2 :: r._1, c)
      })._1.reverse
    } match {
      case x if count < n => doRotation(x, count + 1)
      case x => x
    }

    doRotation(ls, 1)
  }

  def rotate[T](n: Int, list: List[T]): List[T] = {
    val ValidRotation = if (n.abs % list.length == 0) n else n.abs % list.length
    if (n < 0)
      rotateRight(ValidRotation, list)
    else
      rotateLeft(ValidRotation, list)
  }


  println(rotate(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9)))

  println(rotate(3, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")))


}
