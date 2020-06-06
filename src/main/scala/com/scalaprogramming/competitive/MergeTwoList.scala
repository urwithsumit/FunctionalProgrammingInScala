package com.scalaprogramming.competitive

object MergeTwoList {

  def main(args: Array[String]): Unit = {

    def testData(l1: List[Int]) = {
      val nodes = l1 map { x => new ListNode(x) }
      for (i <- 1 until nodes.size) yield {
        nodes(i - 1).next = nodes(i)
      }
      nodes
    }.head


    def printList(out: ListNode) = {
      var X = out
      while (X.next != null) {
        print(s"${X.x} -> ")
        X = X.next
      }
      print(s"${X.x} -> ")
      println("")
    }

    printList(mergeTwoLists(testData(List(1)), testData(List(1))))
  }



  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {

    val head = new ListNode(Int.MaxValue)
    val tail = head

    @scala.annotation.tailrec
    def _mergeList(l1: ListNode, l2: ListNode, curr: ListNode): ListNode = {
      (l1, l2) match {
        case (null, null) => head
        case (l1, null) => curr.next = l1; head
        case (null, l2) => curr.next = l2; head
        case (l1, l2) if l1.x >= l2.x => curr.next = l2; _mergeList(l1, l2.next, curr.next)
        case (l1, l2) if l1.x < l2.x => curr.next = l1; _mergeList(l1.next, l2, curr.next)
      }
    }

    _mergeList(l1, l2, tail)

    head.next

  }

  class ListNode(var _x: Int) {
    var next: ListNode = null
    var x: Int = _x

  }

}
