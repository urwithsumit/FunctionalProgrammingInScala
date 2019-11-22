package com.scalaprogramming.fundamentals

/**
 *
 * Find out whether a ls is a palindrome.
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

    var startPtr = 0
    var endPtr = list.length - 1
    var result = false
    breakable {
      while (startPtr < endPtr) {
        if (list(startPtr) == list(endPtr)) {
          startPtr += 1
          endPtr -= 1
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
