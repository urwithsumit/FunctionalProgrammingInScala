package com.scalaprogramming.competitive

import scala.annotation.tailrec

object PrintReverse {

  def main(args: Array[String]): Unit = {
    assert(revPrint("sumit") == "timus")
    assert(revPrint2("sumit") == "timus")
    assert(revPrint("s") == "s")
    assert(revPrint2("s") == "s")
    assert(revPrint("") == "")
    assert(revPrint2("") == "")
    var s = "1234".toCharArray
    reverseString(s)
    println(s.mkString(","))

  }

  /**
    * via Pattern Matching. Not in place substitution.
    *
    * @param s
    * @return
    */
  def revPrint(s: String): String = {
    @tailrec
    def rev(str: List[Char], acc: String): String = {
      str match {
        case Nil => acc
        case head :: tail => rev(tail, head + acc)
      }
    }

    rev(s.toList, "")
  }

  /**
    *
    * @param s
    * @return
    */
  def revPrint2(s: String): String = {

    def revIdx(idx: Int, acc: String): String = {
      if (idx == s.length) acc
      else revIdx(idx + 1, s.charAt(idx) + acc)
    }

    revIdx(0, s"")
  }

  /**
    * Recursive, in place reverse.
    */
  def reverseString(s: Array[Char]): Unit = {
    val limit = s.length / 2
    def revIdx(idx: Int): Unit = {
      def swap = {
        val tmp = s(idx)
        s(idx) = s(s.length - 1 - idx)
        s(s.length - 1 - idx) = tmp
      }

      if (idx == limit) return
      else {
        swap
        revIdx(idx + 1)
      }
    }

    revIdx(0)
  }

  /**
    * Iterative recursively, new string created.
    *
    * @param s
    * @return
    */
  def reverseString2(s: Array[Char]) = {

    def revIdx(idx: Int, acc: String): String = {
      if (idx == s.length) acc
      else revIdx(idx + 1, s(idx) + acc)
    }

    revIdx(0, s"")
  }


}
