package com.scalaprogramming.competitive

object RangeBitWiseAnd {

  def rangeBitwiseAnd1(m: Int, n: Int): Int = {
    if (m == 0) 0 else {
      var ans = m
      for (i <- m + 1 to n) {
        print(s" a: $ans = ${ans.toBinaryString}")
        print(s" b: $i = ${i.toBinaryString}")
        ans += ans & i
        print(s" ans is $ans = ${ans.toBinaryString}")
        println()
      }
      ans
    }
  }



  def main(args: Array[String]): Unit = {
    //println(rangeBitwiseAnd1(5, 100))

  }


}
