package com.practice.leetcode

object Add2Number {

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

    //println(Link1.head.toString)
    Link1.head
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {

    def addNumber(x: ListNode, y: ListNode, Carry: Int): ListNode = {
      (x, y) match {
        case (null, null) => {
          if (Carry > 0) {
            val node = new ListNode(Carry)
            node
          } else null
        }

        case (null, _) => {
          val Z = y._x + Carry
          if (Z > 9) {
            val node = new ListNode(Z % 10)
            node.next = addNumber(x, y.next, Z / 10)
            node
          } else {
            val node = new ListNode(Z)
            node.next = addNumber(x, y.next, 0)
            node
          }
        }

        case (_, null) => {
          val Z = x._x + Carry
          if (Z > 9) {
            val node = new ListNode(Z % 10)
            node.next = addNumber(x.next, y, Z / 10)
            node
          } else {
            val node = new ListNode(Z)
            node.next = addNumber(x.next, y, 0)
            node
          }
        }

        case (_, _) => {
          val Z = x._x + y._x + Carry
          if (Z > 9) {
            val node = new ListNode(Z % 10)
            node.next = addNumber(x.next, y.next, Z / 10)
            node
          } else {
            val node = new ListNode(Z)
            node.next = addNumber(x.next, y.next, 0)
            node
          }
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
