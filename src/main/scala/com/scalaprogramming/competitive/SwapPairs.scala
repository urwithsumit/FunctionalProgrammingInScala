package com.scalaprogramming.competitive

import scala.collection.immutable

/**
  * https://leetcode.com/problems/swap-nodes-in-pairs/
  */

object SwapPairs {

  def testData[T](list: List[T]) = {
    val nodes = list map { x => new ListNode(x) }
    for (i <- 1 until nodes.size) yield {
      nodes(i - 1).next = nodes(i)
    }
    nodes
  }

  def printList(out: ListNode[Int]) = {
    var X = out
    while (X.next != null) {
      print(s"${X.x} -> ")
      X = X.next
    }
    print(s"${X.x} ")
    println("")
  }


  def main(args: Array[String]): Unit = {

    val nodes: immutable.List[ListNode[Int]] = testData(List(1, 3))
    printList(nodes.head)
    printList(swapPairs(nodes.head))

  }


  def swapPairs[T](h: ListNode[T]): ListNode[T] = {
    if (h == null) return null
    (h, h.next) match {
      case (h, null) => h
      case (x1, x2) =>
        x1.next = swapPairs(x2.next)
        x2.next = x1
        x2
    }
  }


  class ListNode[T](var _x: T) {
    var next: ListNode[T] = null
    var x: T = _x

    implicit def order[T: Ordering]: Ordering[ListNode[T]]  = Ordering.by(_.x)
  }

}
