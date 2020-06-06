import com.scalaprogramming.competitive._
import com.scalaprogramming.competitive.LeetCodeUtility._

import scala.collection.mutable

val copy = Array(
  Array(1, 3, 1),
  Array(1, 5, 1)).copyToArray(Array.ofDim[Int](3,3))

/**
  *
  * Giving up on this problem..
  * My solution does not work and the provided solution in leet code does not rhyme to me.
  *
  * https://leetcode.com/explore/challenge/card/30-day-leetcoding-challenge/529/week-2/3298/
  *
  * @return
  */

def findMaxLength(nums: Array[Int]): Int = {

  nums.indices.map(x => if (nums(x) == 0) nums(x) = -1)

  if (nums.length <= 1) 0 else {
    val groups = for {
      i <- 2 to nums.length
    } yield {
      nums.sliding(i)
    }

    var max = 0
    groups.reverse.flatMap {
      c =>
        c.map {
          arr =>
            if (max > 0) return max
            else if (arr.sum == 0) {
              max = math.max(arr.length, max)
              arr.length
            }
            else 0
        }
    }
    max
  }
}


findMaxLength(Array(0, 0, 0, 1, 1, 0, 1, 1, 1, 0)) //4

findMaxLength(Array(0, 1, 1))
findMaxLength(Array(1))
findMaxLength(Array(0, 1))
findMaxLength(Array(0, 0, 0))

findMaxLength(Array(0, 0, 0, 1, 0, 0, 0, 0, 1))
findMaxLength(Array(0, 0, 1, 0, 0, 0, 1, 1))
findMaxLength(Array(0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1))

findMaxLength(Array(0, 1, 1, 0, 1, 1, 1, 0))

//(-4,2,-2,5) = length

//68
findMaxLength(
  Array(
    0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0,
    1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1,
    1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0,
    1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1,
    0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1,
    1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0,
    0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0,
    1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0,
    1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1,
    1))

findMaxLength(Array(0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1))

findMaxLength(Array(0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0))

/** ********************************************************************/

/**
  * Week 1 Problem: 1 - Single Number
  * Given a non-empty array of integers, every element appears twice except for one. Find that single one.
  *
  * @param nums
  * @return
  */
def singleNumber(nums: Array[Int]): Int = {
  nums.foldLeft(0)((r, c) => {
    println(s"r = ${r.toBinaryString} c =${c.toBinaryString} ::: ${r} ^ ${c} = ${r ^ c}")
    r ^ c
  }
  )
}

singleNumber(Array(1, 2, 1, 2, 4, 3, 3))

/** ********************************************************************/

/**
  * Week 1 Problem: 2 - Happy Number
  * Version 1: Slower
  * Write an algorithm to determine if a number is "happy".
  *
  * A happy number is a number defined by the following process: Starting with any positive integer,
  * replace the number by the sum of the squares of its digits, and repeat the process until the number equals 1
  * (where it will stay), or it loops endlessly in a cycle which does not include 1.
  * Those numbers for which this process ends in 1 are happy numbers.
  *
  * @param n
  * @return
  */

def isHappy1(n: Int): Boolean = {

  import scala.collection.mutable

  def digitsInNumber(num: Int): mutable.Stack[Int] = {
    val stack = mutable.Stack[Int]()
    var x = num
    while (x > 9) {
      stack.push(x % 10)
      x = x / 10
    }
    // Push the last digit
    stack.push(x)

    stack
  }

  def summation(num: Int)(digit: Int => mutable.Stack[Int]): Int = {
    val stack = digit(num)
    var sum = 0
    while (!stack.isEmpty) {
      var y = stack.pop()
      sum = sum + (y * y)
    }

    sum
  }

  def _isHappy(num: Int, acc: Set[Int]): Boolean = {
    val sum = summation(num)(digitsInNumber)
    if (sum == 1) true
    else if (acc.contains(sum)) false
    else _isHappy(sum, acc + sum)
  }

  _isHappy(n, Set())

}

/** ********************************************************************/

/**
  * Week 1 Problem: 2 - Happy Number
  * Version 2: Better and simple way of finding sum of digits of a number.
  *
  * @param n
  * @return
  */

def isHappy(n: Int): Boolean = {

  def _squareOfDigit(n: Int): Int = {
    n match {
      case 0 => 0
      case _ => n.toString.map(c => (c - '0') * (c - '0')).sum
    }
  }

  def _isHappy(n: Int, acc: Set[Int]): Boolean = {
    val sum = _squareOfDigit(n)
    if (sum == 1) true
    else if (acc.contains(sum)) false
    else _isHappy(sum, acc + sum)
  }

  _isHappy(n, Set())

}

isHappy(19)
isHappy(2)
isHappy(8)

/** ********************************************************************/

/**
  * Week 1 Problem: 3 - Maximum Subarray
  * Given an integer array nums, find the contiguous subarray (containing at least one number) which has the largest sum and return its sum.
  *
  * v1.0 Below approach is not optimal
  */

def maxSubArray_v1(nums: Array[Int]): Int = {
  if (nums.length == 1) nums.max else {
    val groups = for {
      i <- 2 to nums.length
    } yield {
      nums.sliding(i)
    }

    scala.math.max(groups.flatMap(c => c.map(_.sum)).max, nums.max)
  }
}

/**
  * Better and faster version.
  *
  * @param nums
  * @return
  */
def maxSubArray(nums: Array[Int]): Int =
// Scan Left will give intermediate results of summation
// Kaden's algorithm
  nums.tail.scanLeft(nums.head)((r, c) => math.max(r + c, c)).max


maxSubArray(Array(-2, 1, -3, 4, -1, 2, 1, -5, 4))
maxSubArray(Array(-1))
maxSubArray(Array(1, 2))

/** ********************************************************************/

/**
  * Week 1 Problem: 4 - Move Zeroes
  *
  * //O(n^2)
  **/
def moveZeroes(nums: Array[Int]): Unit = {
  val limit = nums.length - 1

  def move(idx: Int): Unit = idx match {
    case idx if idx == limit => nums(limit) = 0
    case _ =>
      nums(idx) = nums(idx + 1)
      move(idx + 1)
  }

  for {
    i <- limit to 0 by -1
    if (nums(i) == 0)
  } yield {
    move(i)
  }
}

def moveZeroesOp(nums: Array[Int]): Unit = {

  val pq = mutable.PriorityQueue[Int]().reverse

  for {
    i <- 0 until nums.length
  } yield {
    if (nums(i) != 0) {
      if (pq.length > 0) {
        nums(pq.dequeue()) = nums(i)
        nums(i) = 0
        pq.enqueue(i)
      }
    } else pq.enqueue(i)
  }

}


def moveZeroes2(nums: Array[Int]): Unit = {
  var j = 0
  for {
    i <- 0 until nums.length
  } yield {
    if (nums(i) != 0) {
      val tmp = nums(j)
      nums(j) = nums(i)
      nums(i) = tmp
      j += 1
    }
  }
}

val arr = Array(0, 1, 3, 0, 4, 0, 0, 0, 5, 0)
val arr2 = Array(1, 0, 2)

moveZeroes2(arr)
moveZeroes2(arr2)

println(arr.mkString(","))
println(arr2.mkString(","))

/** ********************************************************************/

/**
  * Week 1 Problem 5: Buy and Sell Stock II
  *
  */

def maxProfit(ints: Array[Int]): Int = {
  if (ints.size < 2) 0
  else {
    {
      ints.indices.tail.foldLeft(0: Int, 0: Int) {
        case ((acc, idx), idx2) =>
          if (ints(idx2) - ints(idx) > 0) (acc + ints(idx2) - ints(idx), idx2)
          else (acc, idx2)
      }
    }._1
  }
}

maxProfit(Array(7, 1, 5, 3, 6, 4)) // -6, 4, -2, 3, -2
maxProfit(Array(1, 2, 3, 4, 5)) // 1, 1, 1, 1
maxProfit(Array(7, 6, 4, 3, 1)) // -1, -2, -1, -2

/** ********************************************************************/

/**
  * Week 1 Problem 6: Group Anagrams
  *
  * @param strs
  * @return
  */
def groupAnagrams(strs: Array[String]): List[List[String]] = {

  import scala.collection.mutable

  {
    strs.foldLeft(mutable.Map[String, List[String]]().empty) {
      case (acc, n) =>
        val K = n.toSeq.sorted.mkString
        val V = acc.getOrElse(K, List[String]().empty)
        acc.addOne(K, V :+ n)
    }
  }.values.toList

}

groupAnagrams(Array("eat",
  "tea",
  "tan",
  "ate",
  "nat",
  "bat",
  "duh", "ill"))

/** ********************************************************************/

/**
  * Week 1 Problem 7: Counting Elements
  */

def countElements(arr: Array[Int]): Int = {
  arr.map { x => arr.contains(x + 1) }.filter(_ == true).size
}

countElements(Array(1, 2, 3))
countElements(Array(1, 1, 3, 3, 5, 5, 7, 7))
countElements(Array(1, 3, 2, 3, 5, 0))
countElements(Array(1, 1, 2, 2))

/** ********************************************************************/

/**
  * Week 2 Problem 1: Middle of the Linked List
  */

def middleNode(head: ListNode): ListNode = {

  var slow = head

  def loop(node: ListNode): ListNode = {
    if (node == null) slow
    else
      (node, node.next) match {
        case (_, null) => slow
        case _ => {
          slow = slow.next
          loop(node.next.next)
        }
      }
  }

  loop(head)
}

printLL(middleNode(makeLL(List(1, 2, 3, 4, 5))))

printLL(middleNode(makeLL(List(1, 2, 3, 4, 5, 6))))

/** *****************************************************************/

/**
  * Week 2 Problem 2: Backspace String Compare
  *
  * https://leetcode.com/explore/challenge/card/30-day-leetcoding-challenge/529/week-2/3291/
  *
  */

def backspaceCompare(S: String, T: String): Boolean = {
  def clean(str: String) = {
    str.foldLeft(new StringBuilder()) {
      (acc, x) =>
        if (x == '#') {
          if (acc.length > 0) acc.deleteCharAt(acc.size - 1) else acc
        } else acc.append(x)
    }
  }

  clean(S) == clean(T)
}

backspaceCompare("ab#c", "ad#c")
backspaceCompare("ab##", "c#d#")
backspaceCompare("a##c", "#a#c")
backspaceCompare("a#c", "b")
backspaceCompare("#", "")

/** *****************************************************************/
class MinStack() {

  var list = scala.collection.mutable.ListBuffer[Int]().empty

  /** initialize your data structure here. */
  def push(x: Int): Unit = {
    list :+= x
  }

  def pop() = {
    if (list.nonEmpty) {
      val l = list.last
      list = list.dropRight(1)
      l
    } else throw new IllegalArgumentException("pop(): Empty Stack")
  }

  def top(): Int = {
    if (list.nonEmpty) list.last else throw new IllegalArgumentException("top(): Empty Stack")
  }

  def getMin(): Int = {
    if (list.nonEmpty) list.min else throw new IllegalArgumentException("getMin(): Empty Stack")
  }

}

var obj = new MinStack()
obj.push(10)
//obj.pop()
var param_3 = obj.top()
var param_4 = obj.getMin()

/** ********************************************************************/

def diameterOfBinaryTree(node: TreeNode): Int = {

  def _height(node: TreeNode): Int = {
    if (node == null) 0
    else math.max(_height(node.left), _height(node.right)) + 1
  }

  def _diameter(node: TreeNode): Int = {
    if (node == null || node.left == null && node.right == null) 0
    else math.max(
      _height(node.left) + _height(node.right),
      math.max(_diameter(node.left), _diameter(node.right)
      ))
  }

  _diameter(node)
}

/** ********************************************************************/

/** *
  * */
def lastStoneWeight(stones: Array[Int]): Int = {

  val _stones = stones.toBuffer

  def getMax = {
    _stones.remove(_stones.indexOf(_stones.max))
  }

  while (_stones.size > 2) {
    val z = getMax - getMax
    if (z > 0) _stones.addOne(z)
  }

  if (_stones.length == 2) (getMax - getMax) else getMax

}

lastStoneWeight(Array(1, 3))
lastStoneWeight(Array(1, 2, 3, 4, 5))
lastStoneWeight(Array(316, 157, 73, 106, 771, 828, 46, 212, 926, 604, 600, 992, 71, 51,
  477, 869, 425, 405, 859, 924, 45, 187, 283, 590, 303, 66, 508, 982, 464, 398))

/** ********************************************************************/

/** *
  * Imperative Approach
  */
def stringShift_imperative_style(s: String, shift: Array[Array[Int]]): String = {

  var left, right = 0
  for (i <- shift) {
    if (i(0) == 0) left += i(1) else right += i(1)
  }

  val factor = math.abs(left - right) % s.length

  if (left > right) {
    s"${s.substring(factor, s.length)}${s.substring(0, factor)}"
  } else {
    s"${s.substring(s.length - factor, s.length)}${s.substring(0, s.length - factor)}"
  }
}

/**
  * Functional Approach
  */
def stringShift_functional_style(s: String, shift: Array[Array[Int]]): String = {

  val acc = shift.groupMapReduce(arr => arr(0))(arr => arr(1))(_ + _)

  val left = acc.getOrElse(0, 0)
  val right = acc.getOrElse(1, 0)

  val factor = math.abs(left - right) % s.length

  if (left > right) {
    s"${s.substring(factor, s.length)}${s.substring(0, factor)}"
  } else {
    s"${s.substring(s.length - factor, s.length)}${s.substring(0, s.length - factor)}"
  }
}


stringShift_functional_style(s = "abc", shift = Array(Array(0, 1), Array(1, 2)))
stringShift_functional_style("yisxjwry", Array(Array(1, 8), Array(1, 4), Array(1, 3), Array(1, 6), Array(0, 6), Array(1, 4), Array(0, 2), Array(0, 1)))

/** ********************************************************************/

/**
  * 238. Product of Array Except Self
  * https://leetcode.com/problems/product-of-array-except-self/
  *
  * Slow Algorithm, a better one was
  *
  * @param nums
  * @return
  */
def productExceptSelf(nums: Array[Int]): Array[Int] = {
  if (nums.length < 1) nums
  else {
    nums.indices.foldLeft(Array[Int]()) {
      (acc, idx) =>
        acc :+ (nums.slice(0, idx).product * nums.slice(idx + 1, nums.length).product)
    }
  }
}

val tArr = Array(1, 2, 3, 4)

tArr.slice(0, 0).product * tArr.slice(1, tArr.length).product

productExceptSelf(Array(1, 2, 3, 4))

/** ********************************************************************/

/** **
  * Valid Paranthesis
  *  `*` can be used as an ( or ) paranthesis or as a single character as well.
  */

def checkValidString(s: String): Boolean = {

  type State = (Char, Int)

  val stack = scala.collection.mutable.Stack[State]().empty
  val setOfStar = scala.collection.mutable.ListBuffer[Int]().empty

  for (i <- s.zipWithIndex) {
    if (!stack.isEmpty) {
      (stack.top, i._1) match {
        case (top, curr) if top._1 == '(' && curr == ')' => stack.pop()
        case (top, curr) if (top._1 == ')' && (curr == '(')) => stack.push(i)
        case (_, curr) if curr == '*' => setOfStar.addOne(i._2) // If Star, than I am keeping its index.
        case _ => stack.push(i)
      }
    } else {
      // If Star, than I am keeping its index.
      if (i._1 == '*') setOfStar.addOne(i._2)
      else stack.push(i)
    }
  }

  if (!stack.isEmpty) {
    var flag = true
    while (!stack.isEmpty && flag) {
      stack.pop match {
        // for ( look for * index greater than index of (
        case (par, idx) if par == '(' && setOfStar.find(_ > idx).getOrElse(-1) > -1 =>
          setOfStar.remove(setOfStar.indexOf(setOfStar.find(_ > idx).get))
        // for ) look for * index lesser than index of (
        case (par, idx) if par == ')' && setOfStar.find(_ < idx).getOrElse(-1) > -1 =>
          setOfStar.remove(setOfStar.indexOf(setOfStar.find(_ < idx).get))
        // No match, lets set the flag to exit from while loop.
        case (par, _) => flag = false
      }
    }

    flag
  } else stack.isEmpty
}

checkValidString("()")
checkValidString("(*)")
checkValidString("(*))")
checkValidString("(*))*(")

/** ********************************************************************/

/**
  * https://leetcode.com/explore/featured/card/30-day-leetcoding-challenge/530/week-3/3304/
  * Search in a sorted array which is rotated at an index.
  * @param num
  * @param target
  * @return
  */
def search(num: Array[Int], target: Int): Int = {

  def arrayMid(low: Int, hi: Int): Int = {
    val x = (hi + low) / 2
    if (x % 2 == 0) x + 1 else x

  }

  def between(low: Int, hi: Int): Boolean = {
    if (low < hi && low > -1)
      num(low) < target && target < num(hi)
    else false
  }

  def searchSplitIdx(low: Int, hi: Int): Int = {
    if (low > hi) -1
    else if (low == hi && low >= 0) low
    else {
      val mid = arrayMid(low, hi)

      if (arr(mid - 1) > arr(mid)) mid // mid point test
      else {
        math.max(searchSplitIdx(low, mid - 1),
          searchSplitIdx(mid, hi))
      }
    }
  }

  def _search(low: Int, hi: Int): Int = {
    if (low < hi) {
      val mid = arrayMid(low, hi)

      if (mid < num.length && num(mid) == target) mid
      else if (between(low, mid - 1)) _search(low, mid - 1)
      else if (between(mid, hi)) _search(mid, hi)
      else -1

    } else -1

  }

  val idx = searchSplitIdx(0, num.length)

  math.max(_search(0, idx), _search(idx + 1, num.length))

}

search(Array(4, 5, 6, 7, 0, 1, 2), 0)

//search(Array(4, 5, 6, 7, 0, 1, 2), 3)
//search(Array(1), 2)
//search(Array(1), 1)

/** ********************************************************************/

/** *
  * https://leetcode.com/explore/challenge/card/30-day-leetcoding-challenge/530/week-3/3306/
  *
  * @param arr
  */

class BinaryMatrix(arr: Array[Array[Int]], m: Int, n: Int) {
  def get(x: Int, y: Int): Int = {
    arr(x)(y)
  }

  def dimensions(): Array[Int] = Array(m, n)
}

def leftMostColumnWithOne(binaryMatrix: BinaryMatrix): Int = {

  val row :: column :: Nil = binaryMatrix.dimensions().toList

  var i = row - 1
  var j = column - 1

  while (i >= 0 && j >= 0) {
    if (binaryMatrix.get(i, j) == 0) i -= 1
    else j -= 1
  }

  // Since rows are sorted, when there are no ones,
  // the pointer will not jump to next column and will be equal to the size of column
  if (j + 1 == column) -1 else j + 1

}

//0  Ans: 1
leftMostColumnWithOne(new BinaryMatrix(Array(
  Array(0, 0, 0, 1),
  Array(0, 0, 1, 1),
  Array(0, 1, 1, 1)), 3, 4))
//1 Ans: 0
leftMostColumnWithOne(new BinaryMatrix(
  Array(
    Array(0, 0, 0, 0),
    Array(1, 1, 1, 1),
    Array(0, 0, 0, 1)), 3, 4))
//2 Ans: 0
leftMostColumnWithOne(new BinaryMatrix(
  Array(
    Array(1, 1, 1, 1),
    Array(0, 0, 0, 0),
    Array(0, 0, 0, 0)), 3, 4))
//3 Ans 3
leftMostColumnWithOne(new BinaryMatrix(Array(
  Array(0, 0, 0, 1),
  Array(0, 0, 0, 0),
  Array(0, 0, 0, 0)), 3, 4))
//4 Ans 1
leftMostColumnWithOne(new BinaryMatrix(Array(
  Array(0, 1, 1, 1),
  Array(0, 0, 0, 0),
  Array(0, 0, 1, 0)), 3, 4))
//5 Ans 2
leftMostColumnWithOne(new BinaryMatrix(Array(
  Array(0, 0, 1, 1),
  Array(0, 0, 0, 0),
  Array(0, 0, 1, 0)), 3, 4))
//6 Ans -1
leftMostColumnWithOne(new BinaryMatrix(Array(
  Array(0, 0, 0, 0),
  Array(0, 0, 0, 0),
  Array(0, 0, 0, 0)), 3, 4))

//******************************************************

def bstFromPreorder(arr: Array[Int]): TreeNode = {

  def loop(root: TreeNode, preOrder: List[Int]): TreeNode = {
    preOrder.span(_ < root.value) match {
      case (Nil, Nil) =>
      case (lh :: lt, Nil) => root.left = new TreeNode(lh)
        loop(root.left, lt)
      case (Nil, rh :: rt) => root.right = new TreeNode(rh)
        loop(root.right, rt)
      case (lh :: lt, rh :: rt) => {
        root.left = new TreeNode(lh)
        loop(root.left, lt)
        root.right = new TreeNode(rh)
        loop(root.right, rt)
      }
    }

    root
  }

  if (!arr.isEmpty) {
    loop(new TreeNode(arr.head), arr.tail.toList)
  } else null
}

bstFromPreorder(Array(8, 5, 1, 7, 10, 12)).toString

//******************************************************
/**
  * Subarray Sum Equals K
  *
  * @param nums
  * @param k
  * @return
  */
def subarraySum(nums: Array[Int], k: Int): Int = {
  //scan Left gives the cumulative totals, than perform fold left on the array of these totals.
  nums.scanLeft(0)(_ + _).foldLeft(Map[Int, Int](), 0: Int) {
    case ((preTotal, counter), sum) =>
      if (preTotal.contains(sum - k))
      // Make an entry or increment the existing entry for the sum encountered per iteration.
      // Increment the counter by the total occurrences for (sum - k) i.e. the number of sub arrays already encountered.
      (preTotal + (sum -> (preTotal.getOrElse(sum, 0) + 1)), counter + preTotal.getOrElse(sum - k, 0))
      else (preTotal + (sum -> (preTotal.getOrElse(sum, 0) + 1)), counter)
  }._2
}

subarraySum(Array(1, -1, 1, 2, 1, 2), 0)
subarraySum(Array(-1, -1, 1, 2, 1, 2), -1)
subarraySum(Array(-1, -1, 1, 2, 1, 2), -2)
subarraySum(Array(-1, -1, 1, 2, 1, 2), 1)
subarraySum(Array(-1, -1, 1, 2, 1, 2), 2)
subarraySum(Array(-1, -1, 1, 2, 1, 2), 3)
subarraySum(Array(-1, -1, 1, 2, 1, 2), 4)
subarraySum(Array(-1, -1, 1, 2, 1, 2), 6)
subarraySum(Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 0)

//******************************************************//

def rangeBitwiseAnd(m: Int, n: Int): Int = {
  if (m == n) m else {
    (m + 1 to n).foldLeft(m) {
      (r, c) => if ((r & c) == 0) return 0 else r & c
    }
  }
}
println(rangeBitwiseAnd(5, Int.MaxValue))
println(rangeBitwiseAnd(0, 1))
println(rangeBitwiseAnd(1, 1))
println(rangeBitwiseAnd(2, 1))

//******************************************************//

/**
* Merge K Sorted List.
*
*/
def mergeKLists(lists: Array[ListNode]): ListNode = {
  if (!lists.isEmpty) {
    import scala.collection.mutable._

    // Add content of list to Queue
    val queue = PriorityQueue[ListNode]()(Ordering.by[ListNode, Int](_.x).reverse) ++ lists.filter(_ != null)

    val head = new ListNode(Int.MinValue)

    // Base case - Update Head
    if (!queue.isEmpty) head.next = queue.dequeue()
    var curr = head
    addToQueue

    def addToQueue = {
      if (curr != null && curr.next != null) {
        curr = curr.next // move the curr pointer to the newly added node.
        if (curr.next != null)
          queue.addOne(curr.next) // add the next of node to Queue, as it is still to be evaluated.
      }
    }

    while (!queue.isEmpty) {
      curr.next = queue.dequeue()
      addToQueue
    }

    head.next
  } else null
}

LeetCodeUtility.printLL(
  mergeKLists(Array(LeetCodeUtility.makeLL(List(9, 9, 9).sorted),
    LeetCodeUtility.makeLL(List(0, 0, 0).sorted),
    LeetCodeUtility.makeLL(List(100, -1, 10).sorted),
    LeetCodeUtility.makeLL(List(7, 4, 3, 1).sorted)
  )))

mergeKLists(Array[ListNode]())

//******************************************************//
/**
*
*/

def longestCommonSubsequence(text1: String, text2: String): Int = {

  if (text1.isEmpty || text2.isEmpty) return 0

  val t1Arr = text1.toCharArray
  val t2Arr = text2.toCharArray

  val visited = scala.collection.mutable.Map[(Int, Int), Int]()

  def _cache(key: (Int, Int)): Int = {
    if (!visited.contains(key)) {
      val len = _lcs(key._1, key._2)
      visited.addOne(key, len)
    }
    visited.getOrElse(key, -1)
  }

  def _lcs(t1: Int, t2: Int): Int = {
    (t1, t2) match {
      case (x, y) if (y >= t2Arr.size || x >= t1Arr.size) => 0
      case _ if visited.contains(t1, t2) => _cache((t1, t2)) + 1
      case _ if (t1Arr(t1) == t2Arr(t2)) => _cache((t1 + 1, t2 + 1)) + 1
      case _ => math.max(_cache((t1, t2 + 1)), _cache((t1 + 1, t2)))
    }
  }

  _lcs(0, 0)
}

println(longestCommonSubsequence("abcde", "ace"))
println(longestCommonSubsequence("abc", "abc"))
println(longestCommonSubsequence("abc", "def"))
println(longestCommonSubsequence("mhunuzqrkzsnidwbun", "szulspmhwpazoxijwbq"))


////////////////////////////////////////////////////////////////////////////////

/**
* Count the number of Islands...
*/
def numIslands(matrix: Array[Array[Char]]): Int = {

  if (matrix.isEmpty) return 0

  val visited = scala.collection.mutable.Set[(Int, Int)]()

  /**
  * Eligible when index has a value of 1 and index is not already visited
  */
  def isEligibleIdx(r: Int, c: Int): Boolean = {
    r > -1 && r < matrix.length && c > -1 && c < matrix(r).length && matrix(r)(c) == '1' && !visited.contains((r, c))
  }

  /**
  * Use DFS to Explore the island at the given entry point.
  */
  def exploreIsland(row: Int, column: Int) = {
    val stack = scala.collection.mutable.Stack[(Int, Int)]()
    stack.addOne((row, column))

    while (!stack.isEmpty) {
      val (r, c) = stack.pop()

      if (isEligibleIdx(r, c)) {
        // Add to visited Set
        visited.addOne((r, c))

        //Add all neighbour to Stacks.
        stack.addOne((r, c + 1)) // EAST
        stack.addOne((r, c - 1)) // WEST
        stack.addOne((r + 1, c)) // NORTH
        stack.addOne((r - 1, c)) // SOUTH
      }
    }
  }

  val Islands = for {
    row <- 0 until matrix.length
    column <- 0 until matrix(row).length
    if isEligibleIdx(row, column)
  } yield {
    exploreIsland(row, column)
    (row, column)
  }

  Islands.length
}

numIslands(Array(
  "11110".toCharArray,
  "11010".toCharArray,
  "11000".toCharArray,
  "00000".toCharArray))

numIslands(Array("11000".toCharArray,
  "11000".toCharArray,
  "00100".toCharArray,
  "00011".toCharArray))

numIslands(Array("".toCharArray,
  "".toCharArray,
  "".toCharArray))

////////////////////////////////////////////////////////////////////////////////

//https://leetcode.com/explore/challenge/card/30-day-leetcoding-challenge/530/week-3/3303/

def minPathSum(grid: Array[Array[Int]]): Int = {
  if (grid.isEmpty) return 0

  for {
    row <- 0 until grid.length
    col <- 0 until grid(row).length
    if !(row == 0 && col == 0)
  } {
    val up = if (row > 0) grid(row - 1)(col) else Int.MaxValue
    val left = if (col > 0) grid(row)(col - 1) else Int.MaxValue
    grid(row)(col) += math.min(up, left)
  }

  //print(grid.map(x => x.mkString(", ")).mkString("\n"))

  grid.last.last
}


minPathSum(Array(
  Array(1, 3, 1),
  Array(1, 5, 1),
  Array(4, 2, 1)
))

minPathSum(Array(
  Array(1, 2),
  Array(1, 1)
))

minPathSum(Array(
  Array(0, 7, 7, 8, 1, 2, 4, 3, 0, 0, 5, 9, 8, 3, 6, 5, 1, 0),
  Array(2, 1, 1, 0, 8, 1, 3, 3, 9, 9, 5, 8, 7, 5, 7, 5, 5, 8),
  Array(9, 2, 3, 1, 2, 8, 1, 2, 3, 7, 9, 7, 9, 3, 0, 0, 3, 8),
  Array(3, 9, 3, 4, 8, 1, 2, 6, 8, 9, 3, 4, 9, 4, 8, 3, 6, 2),
  Array(3, 7, 4, 7, 6, 5, 6, 5, 8, 6, 7, 3, 6, 2, 2, 9, 9, 3),
  Array(2, 3, 1, 1, 5, 4, 7, 4, 0, 7, 7, 6, 9, 1, 5, 5, 0, 3),
  Array(0, 8, 8, 8, 4, 7, 1, 0, 2, 6, 1, 1, 1, 6, 4, 2, 7, 9),
  Array(8, 6, 6, 8, 3, 3, 5, 4, 6, 2, 9, 8, 6, 9, 6, 6, 9, 2),
  Array(6, 2, 2, 8, 0, 6, 1, 1, 4, 5, 3, 1, 7, 3, 9, 3, 2, 2),
  Array(8, 9, 8, 5, 3, 7, 5, 9, 8, 2, 8, 7, 4, 4, 1, 9, 2, 2),
  Array(7, 3, 3, 1, 0, 9, 4, 7, 2, 3, 2, 6, 7, 1, 7, 7, 8, 1),
  Array(4, 3, 2, 2, 7, 0, 1, 4, 4, 4, 3, 8, 6, 2, 1, 2, 5, 4),
  Array(1, 9, 3, 5, 4, 6, 4, 3, 7, 1, 0, 7, 2, 4, 0, 7, 8, 0),
  Array(7, 1, 4, 2, 5, 9, 0, 4, 1, 4, 6, 6, 8, 9, 7, 1, 4, 3),
  Array(9, 8, 6, 8, 2, 6, 5, 6, 2, 8, 3, 2, 8, 1, 5, 4, 5, 2),
  Array(3, 7, 8, 6, 3, 4, 2, 3, 5, 1, 7, 2, 4, 6, 0, 2, 5, 4),
  Array(8, 2, 1, 2, 2, 6, 6, 0, 7, 3, 6, 4, 5, 9, 4, 4, 5, 7)))


////////////////////////////////////////////////////////////////////////////////

object Solution {

  def isValidSequence(root: TreeNode, arr: Array[Int]): Boolean = {

    def isLeaf(node: TreeNode) = node != null && node.left == null && node.right == null

    def dfs(root: TreeNode, arr: List[Int]): Boolean = {
      arr match {
        // if leaf is reached, than the array size should have only the node value.
        case head :: Nil if root != null && root.value == head => isLeaf(root)
        case head :: tail if root != null && root.value == head =>
          // either of left or right will have the sequence.
          dfs(root.left, tail) || dfs(root.right, tail)
        case _ => false
      }
    }

    dfs(root, arr.toList)
  }
}
