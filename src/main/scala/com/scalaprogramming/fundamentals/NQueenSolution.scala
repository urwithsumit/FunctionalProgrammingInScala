package com.scalaprogramming.fundamentals

object NQueenSolution extends App {


  val Solutions = queens(6)

  def queens(n: Int): List[List[(Int, Int)]] = {

    def placeQueens(k: Int): List[List[(Int, Int)]] = {
      println(s"Inside Place Queens: ${k}")
      if (k == 0)
        List(List())
      else
        for {
          queens <- placeQueens(k - 1)
          column <- 1 to n
          queen = (k, column)
          if isSafe(queen, queens)

        } yield {
          val Q = queen :: queens
          println(s"Appending to Solution: ${queen}. Now: $Q")
          Q
        }
    }

    placeQueens(n)

  }

  def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) = {
    println(s"Inside isSafe: ${queen}. Possible Queens are: ${queens.size}")
    queens forall (q => !inCheck(queen, q))
  }

  def inCheck(q1: (Int, Int), q2: (Int, Int)) =
    q1._1 == q2._1 ||
    q1._2 == q2._2 ||
    (q1._1 - q2._1).abs == (q1._2 - q2._2).abs

  println(Solutions.size)
  println(s"Final Solution: $Solutions ")

}
