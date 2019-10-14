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

  def isPalindrome(s: String): Boolean = {
    val cleanStr = s.replaceAll("[^0-9a-zA-Z]", "").toLowerCase
    for ( i <- 0 until cleanStr.length/2) {
      if (cleanStr.charAt(i) != cleanStr.charAt(cleanStr.length - i - 1))
        return false
    }
    return true
  }

  def isPalindrome[T](list: List[T]): Boolean = {
    import util.control.Breaks._

    val length = list.length
    val limit = if (length % 2 == 0) length / 2 else length / 2 + 1
    var i = 0
    var j = length - 1
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
