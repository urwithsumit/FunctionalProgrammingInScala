import com.scalaprogramming.competitive.LeetCodeUtility._

import scala.annotation.tailrec

def adder(m: Int, n: Int) = m + n

/**
  * https://leetcode.com/explore/learn/card/recursion-i/253/conclusion/1675/
  *
  * @param row
  * @param K
  * @return
  */
def kthGrammar(row: Int, K: Int): Int = {

  def encode(str: String): String = {
    val conv = if (str.isEmpty) "0" else str map (x => if (x == '0') "1" else "0") mkString ("")
    println(conv)
    conv
  }

  @tailrec
  def _kth(i: Int, acc: String = ""): String = {
    println(s"i = $i, acc = ${acc}")
    i match {
      case i if i == row => acc + encode(acc)
      case _ => _kth(i + 1, acc + encode(acc))
    }
  }

  // Index starts at 1. For the 1st Index, acc is set to 0.
  // So, invoke the _kth with 2nd Index and acc as 0
  if (_kth(0).charAt(K - 1) == '0') 0 else 1
}

/*kthGrammar(1, 1)
kthGrammar(2, 1)
kthGrammar(2, 2)
kthGrammar(4, 5)*/
//kthGrammar(5, 8)
//kthGrammar(30, 434991989)

/** ********************************************************/

val x = 8 % 10
val y = 18 % 10

val xmod = 8 / 10
val ymod = 18 / 10

/** ********************************************************/
/**
  * Leet Code Problem from Recursive Set
  *
  * @param stairs
  * @return
  */

def climbStairs(stairs: Int): Int = {

  def climb(i: Int = 0, acc: List[Int] = List(0, 1, 2)): List[Int] = {
    i match {
      case y if y <= 2 => climb(i + 1, acc)
      case y if y <= stairs => climb(i + 1, acc :+ acc(i - 2) + acc(i - 1))
      case _ => acc
    }
  }

  climb()(stairs)
}

climbStairs(2)
climbStairs(5)
climbStairs(1000)

/** ********************************************************/
/**
  * Fibonacci from Leet Code Recursive Set
  *
  * @param N
  * @return
  */
def fib(N: Int): Int = {
  lazy val fib: LazyList[Int] = {
    def loop(h: Int, n: Int): LazyList[Int] = h #:: loop(n, h + n)

    loop(0, 1)
  }

  fib.take(N + 1).last
}

fib(3)

/** ********************************************************/
/**
  * Leet Code Recursion Set - Find max Depth of a Tree
  *
  * @param
  */
def maxDepth(root: TreeNode): Int = {

  def _maxDep(root: TreeNode, depth: Int): Int = {
    if (root == null) depth
    else Math.max(_maxDep(root.left, depth + 1), _maxDep(root.right, depth + 1))
  }

  _maxDep(root, 0)
}

val s = "hello"
1 / 2 / 2
math.abs(Int.MinValue)

Int.MinValue * -1

/** ********************************************************/
/**
  * Leet Code - Find Pow of a Number.
  *
  * @param x
  * @param n
  * @return
  */
def myPow(x: Double, n: Int): Double = {

  def _myPow(y: Double, z: Int): Double = {
    if (y == 0) 0
    else if (z == 0) 1
    else if (z == 1) y
    else if (z % 2 == 0) _myPow(y * y, z / 2)
    else (_myPow(y * y, (z - 1) / 2)) * y
  }

  // math.abs(Int.MinValue) returns Int.MinValue, hence we explicitly set power to Int.MaxValue
  // Since MaxValue is numerically 1 less than MinValue, we will factor this in next steps.
  // Note: We cannot add 1 to Int.MaxValue as it will return Int.MinValue.
  val power = if (n == Int.MinValue) Int.MaxValue else math.abs(n)

  // Calculate intermediate Result
  val _ans = _myPow(x, power)

  // Now adjust the intermediate result for the additional power for Int.MinValue case
  val ans = if (n == Int.MinValue) x * _ans else _ans

  // Final answer will factor if the power was positive or negative.
  if (n < 0) 1.0 / ans else ans

}


myPow(2.00000, -2147483648)
myPow(-1.00000, -2147483648)
myPow(2, 0) // 1
myPow(2, 1) // 2
myPow(2, 2) // 4
myPow(2, 3) // 8
myPow(2, 4) // 16
myPow(2, 5) // 32
myPow(2, 6) // 64
myPow(2, 7) // 128
myPow(2, 8) // 256
myPow(2, 9) // 512
myPow(2, 10) // 1024
myPow(2, -1) // 0.5
myPow(0, -4)


/** ********************************************************/
/**
  * K-th Symbol in Grammar
  *
  * https://leetcode.com/explore/learn/card/recursion-i/253/conclusion/1675/
  * // TODO Times out for kthGrammar2(30, 434991989)
  *
  * @param row
  * @param K
  * @return
  */
def kthGrammar2(row: Int, K: Int): Int = {

  def flipBits(str: String): String =
    if (str.isEmpty) "0"
    else str map (x => if (x == '0') "1" else "0") mkString ("")

  def _kth(i: Int, right: String = ""): String = {

    println(s"i = $i, right = ${right}")

    i match {
      case i if i == row => right.charAt(K - 1).toString
      case _ => _kth(i + 1, right + flipBits(right))
    }
  }

  // Index starts at 1. For the 1st Index, acc is set to 0.
  // So, invoke the _kth with 2nd Index and acc as 0
  if (_kth(0) == "0") 0 else 1

}

kthGrammar2(1, 1)
kthGrammar2(2, 1)
kthGrammar2(2, 2)
kthGrammar2(4, 5)
//kthGrammar2(30, 434991989)
/** ********************************************************/