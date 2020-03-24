package com.scalaprogramming.competitive

/**
  * https://leetcode.com/problems/nim-game/
  *
  * 'A' will loose whenever stones are in multiple of 4.
  */
object NimGame {

  def main(args: Array[String]) = {
    assert(canWinNim(1) == true)
    assert(canWinNim(2) == true)
    assert(canWinNim(3) == true)
    assert(canWinNim(4) == false)
    assert(canWinNim(5) == true)
    assert(canWinNim(6) == true)
    assert(canWinNim(7) == true)
    assert(canWinNim(8) == false)
    assert(canWinNim(9) == true)
  }

  def canWinNim(n: Int): Boolean = {
    n % 4 != 0
  }

}

