package com.scalaprogramming.competitive

class FirstUnique(_nums: Array[Int]) {

  private lazy val frqMap = scala.collection.mutable.LinkedHashMap[Int, Int]()
  private lazy val queue = scala.collection.mutable.ListBuffer[Int]() ++ _nums

  private def updateKeyFreq(key: Int, count: Int) = {
    frqMap.update(key, frqMap.getOrElse(key, 0) + count)
    if (frqMap.getOrElse(key, 0) == 0) frqMap.remove(key)
  }

  private def initialize = {
    for (i <- queue) {
      updateKeyFreq(i, 1)
    }
  }

  private def searchFU: Int = {
    queue.indexWhere(c => frqMap.getOrElse(c, 0) == 1)
  }

  def showFirstUnique(): Int = {
    if (frqMap.isEmpty) initialize

    val idx = searchFU

    if (idx > -1) {
      updateKeyFreq(queue(idx), -1)
      queue.remove(idx)
    } else -1
  }

  def add(value: Int): Unit = {
    if (frqMap.isEmpty) initialize
    queue.addOne(value)
    updateKeyFreq(value, 1)
  }
}

object FirstUniqueTest extends App {

  val firstUnique = new FirstUnique(Array(7, 7, 7, 7, 7, 7))
  firstUnique.showFirstUnique() // return -1
  firstUnique.add(7) // the queue is now Array(7,7,7,7,7,7,7]
  firstUnique.add(3) // the queue is now Array(7,7,7,7,7,7,7,3]
  firstUnique.add(3) // the queue is now Array(7,7,7,7,7,7,7,3,3]
  firstUnique.add(7) // the queue is now Array(7,7,7,7,7,7,7,3,3,7]
  firstUnique.add(17) // the queue is now Array(7,7,7,7,7,7,7,3,3,7,17]
  println(firstUnique.showFirstUnique() )// return 17

}





