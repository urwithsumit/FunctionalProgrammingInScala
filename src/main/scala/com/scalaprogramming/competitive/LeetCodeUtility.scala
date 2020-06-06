package com.scalaprogramming.competitive

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}

class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}


object LeetCodeUtility {

  def makeLL(xs: List[Int]): ListNode = {
    val nodes = xs map { x => new ListNode(x) }
    for (i <- 1 until nodes.size) yield {
      nodes(i - 1).next = nodes(i)
    }
    nodes.head
  }

  def printLL(out: ListNode) = {
    var X = out
    while (X.next != null) {
      print(s"${X.x} -> ")
      X = X.next
    }
    print(s"${X.x} -> ")
    println("")
  }
}
