package com.scalaprogramming.competitive

/**
  * https://leetcode.com/problems/swap-nodes-in-pairs/
  */

object SwapPairs {

  def main(args: Array[String]): Unit = {

    val nodes = List(1, 2, 3, 4, 5, 6, 11, 12, 13, 14) map { x => new ListNode(x) }
    for (i <- 1 until nodes.size) yield {
      nodes(i - 1).next = nodes(i)
    }

    printList(nodes.head)

    def printList(out: ListNode) = {
      var X = out
      while (X.next != null) {
        print(s"${X.x} -> ")
        X = X.next
      }
      print(s"${X.x} -> ")
      println("")
    }

    printList(swapPairs(nodes.head))


  }

  def swapPairs(head: ListNode): ListNode = {
    def swap(h: ListNode): ListNode = {
      if (h == null) return null
      (h, h.next) match {
        case (h, null) => h
        case (x1, x2) =>
          x1.next = swap(x2.next)
          x2.next = x1
          x2
      }
    }

    swap(head)
  }

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }

}
