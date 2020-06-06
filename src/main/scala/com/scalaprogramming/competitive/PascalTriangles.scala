package com.scalaprogramming.competitive

object PascalTriangles {

  def main(args: Array[String]): Unit = {
    println(getRow(3).mkString(","))
  }

  def getRow(rowIndex: Int): List[Int] = {
    if (rowIndex == 0) List(1)
    else {
      val PrevRow = getRow(rowIndex - 1)
      1 +: {
        for {
          i <- 0 until PrevRow.length - 1
        } yield {
          PrevRow(i) + PrevRow(i + 1)
        }
      } :+ 1

    }.toList
  }

}
