package com.scalaprogramming.competitive

/**
  * https://leetcode.com/problems/add-two-numbers/
  *
  * Status: Success
  *
  */

object AddTwoNumbers {

  def main(args: Array[String]) = {
    val l1 = makeLL(Seq(9))
    val l2 = makeLL(Seq(9))

    println(addTwoNumbers(l1, l2))
  }

  def makeLL(n: Seq[Int]) = {
    val n1 = n map { x => new ListNode(x) }

    val Link1 = n1.length match {
      case 0 => n1
      case 1 => n1
      case _ => for {
        i <- n1.length - 1 until 0 by -1
      } yield {
        n1(i).next = n1(i - 1)
        n1(i)
      }
    }

    //println(Link1.current.toString)
    Link1.head
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {

    def addDigitNode(f: (ListNode, ListNode, Int) => ListNode, node1: ListNode, node2: ListNode, resultNode: Int) = {
      val node = new ListNode(resultNode % 10) // storing unit digit
      node.next = f(node1, node2, resultNode / 10) // carry forward for addition
      node
    }

    //Case 1: # of digits in node 1 and node 2 are equal
    //node1 is  null -> 1 -> 2
    //node 2 is  null -> 2 -> 3

    // Case 2: # of digits in node 1 < node 2
    // node 1 is  null -> 1s
    //node 2 is null -> 1 -> 2 -> 3

    // Case 3: # of digits in node 1 > node 2
    // node 1 is null -> 1 -> 2 -> 3
    // node 2 is null -> 1 -> 2
    def addNumber(node1: ListNode, node2: ListNode, Carry: Int): ListNode = {
      (node1, node2) match {
        case (null, null) => {
          if (Carry > 0) new ListNode(Carry)
          else null
        }

        case (null, _) => {
          val Z = node2.x + Carry
          addDigitNode(addNumber, node1, node2.next, Z)
        }

        case (_, null) => {
          val Z = node1.x + Carry
          addDigitNode(addNumber, node1.next, node2, Z)
        }

        case (_, _) => {
          val Z = node1.x + node2.x + Carry
          addDigitNode(addNumber, node1.next, node2.next, Z)
        }
      }
    }

    addNumber(l1, l2, 0)

  }

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x

    override def toString: String = {
      s"_x: ${_x} -> [${if (next != null) next else null}] "
    }
  }

}
