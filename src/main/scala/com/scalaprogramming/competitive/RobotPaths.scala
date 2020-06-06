package com.scalaprogramming.competitive
//https://leetcode.com/problems/unique-paths/submissions/
object RobotPaths {

  def main(args: Array[String]) = {
    assert(uniquePaths2(3, 2) == 3)
    assert(uniquePaths2(7, 3) == 28)
    assert(uniquePaths2(10, 10) == 48620)
    assert(uniquePaths2(23, 13) == 548354040)
    assert(uniquePaths2(4, 4) == 20)
  }

  def uniquePaths2(m: Int, n: Int): Int = {
    val paths = Array.fill(m)(1)
    for (r <- 1 until n; c <- 1 until m) {
      paths(c) = paths(c) + paths(c - 1)
    }
    paths.last
  }

  def uniquePaths(column: Int, row: Int): BigInt = {

    def RIGHT(j: Int) = j + 1

    def DOWN(i: Int) = i + 1

    val result = scala.collection.mutable.Queue[(Seq[String])]()

    def findPaths(x: Int, y: Int, acc: Seq[String]): Unit = {
      if (x == row - 1 && y == column - 1) {
        result.enqueue(acc :+ "END")
      } else {
        if (RIGHT(y) < column) {
          findPaths(x, RIGHT(y), acc :+ "RIGHT")
        }

        if (DOWN(x) < row) {
          findPaths(DOWN(x), y, acc :+ "DOWN")
        }
      }
    }

    findPaths(0, 0, Seq("START"))

    println(s"All result ${result.length}")
    //println(result.mkString("\n"))

    result.length
  }

}
