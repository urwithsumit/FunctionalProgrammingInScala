package com.scalaprogramming.competitive

object ReverseLinkList {

  def main(args: Array[String]): Unit = {

    val nodes = List(1, 2) map { x => new ListNode(x) }
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
      print(s"${X.x} -> NULL ")
      println("")
    }

    printList(reverseList(nodes.head))
  }

  def reverseList(head: ListNode): ListNode = {
    var header = head

    def reverse(head: ListNode): ListNode = {
      if (head == null) return null

      (head, head.next) match {
        case (h, null) => {
          header = h
          h
        }
        case (x1, x2) => {
          //Condition to handle a List of size 2.
          if (x2.next != null) {
            val x3 = reverse(x2.next)
            x3.next = x2
          } else header = x2

          x2.next = x1
          x1.next = null
          x1
        }
      }
    }

    reverse(head)
    header
  }


  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }

}
