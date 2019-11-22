package com.scalaprogramming.fundamentals

/**
  *
  * Find the last but one element of a ls.
  *
  * Example:
  * scala> penultimate(List(1, 1, 2, 3, 5, 8))
  * res0: Int = 5
  *
  */

object SecondToLastElementOfList extends App {

  def secondToLast[T](list: List[T]): T = {
    list match {
      case _2ndLast :: _ :: Nil => _2ndLast
      case _ :: tail => secondToLast(tail)
      case _ => throw new NoSuchElementException
    }
  }

  println(secondToLast (List(1, 1, 2, 3, 5, 8)))
  println(secondToLast (List(1)))
  println(secondToLast (List()))

}
