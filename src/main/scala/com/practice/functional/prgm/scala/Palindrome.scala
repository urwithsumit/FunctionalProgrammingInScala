package com.practice.functional.prgm.scala

/**
 *
 * Find out whether a list is a palindrome.
 * Example:
 * scala> isPalindrome(List(1, 2, 3, 2, 1))
 * res0: Boolean = true
 *
 */
object Palindrome extends App {

  def makeAlphanumeric(s: String, inSensitive: Boolean) = {
    val Clean = {
      if (inSensitive) s.toLowerCase else s
      }.replaceAll("[^a-zA-Z0-9]", "").toList
    println(s"Input: ${s}")
    println(s"Clean Input: $Clean")

    Clean
  }

  def isPalindrome[T](list: List[T]): Boolean = {
    import util.control.Breaks._

    var i = 0
    var j = list.length - 1
    var result = false
    breakable {
      while (i < j) {
        if (list(i) == list(j)) {
          i += 1
          j -= 1
        } else {
          break
        }
      }
      result = true
    }
    result
  }

  println(isPalindrome(List(1, 3, 3, 1)))
  println(isPalindrome(List(1, 2, 3, 1)))
  println(isPalindrome(List(1, 2, 5, 2, 1)))
  println(isPalindrome(List(1)))
  println(isPalindrome(List(1, 3, 5, 3, 1)))
  println(isPalindrome(List("X", "Y", "Z", "Y", "X")))
  println(isPalindrome(List(2, 3, 5, 3, 1)))
  println(isPalindrome(List()))
  println(isPalindrome(List(-1, -2, -5, -2, -1)))
  println(isPalindrome(makeAlphanumeric("A man, a plan, a canal: Panama", true)))
  println(isPalindrome(makeAlphanumeric("0P", true)))
}
