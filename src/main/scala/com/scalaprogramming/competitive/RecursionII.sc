class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

/**
  * Merge sort implementation in a legacy style.
  *
  * @param nums
  * @return
  */
def legacyMergeSort(nums: Array[Int]): Array[Int] = {

  def sort(nums: Array[Int]): Array[Int] = {
    if (nums.length <= 1) nums else {
      val mid = nums.length / 2
      val n1 = sort(nums.slice(0, mid))
      val n2 = sort(nums.slice(mid, nums.length))

      merge(n1, n2)
    }
  }

  def merge(n1: Array[Int], n2: Array[Int]): Array[Int] = {

    val merged = Array.ofDim[Int](n1.length + n2.length)
    var p1 = 0
    var p2 = 0
    var m1 = 0

    while (p1 < n1.length && p2 < n2.length) {
      if (n1(p1) <= n2(p2)) {
        merged(m1) = n1(p1)
        m1 += 1
        p1 += 1
      } else {
        merged(m1) = n2(p2)
        m1 += 1
        p2 += 1
      }
    }

    while (p1 < n1.length) {
      merged(m1) = n1(p1)
      p1 += 1
      m1 += 1
    }

    while (p2 < n2.length) {
      merged(m1) = n2(p2)
      p2 += 1
      m1 += 1
    }

    merged
  }

  sort(nums)
}

legacyMergeSort(Array(-1, 2, 3, 0, 99, 23, 12, 11, 10, 5, 4, 3, 2, -1))

/**********************************************************************/

/**
  * Test is a valid BST.
  *
  * I copied it from another person's solution.
  *
  * @param root
  * @return
  */
def isValidBST(root: TreeNode): Boolean = {

  def _isBST(root: TreeNode, lo: Long, hi: Long): Boolean = {
    root == null ||
      root.value > lo && root.value < hi && _isBST(root.left, lo, root.value) && _isBST(root.right, root.value, hi)
  }

  _isBST(root, Long.MinValue, Long.MaxValue)

}

/**********************************************************************/

/** **
  *
  * TODO - Solution Pending.
  *
  * Search a 2D Matrix II
  * Write an efficient algorithm that searches for a value in an m x n matrix. This matrix has the following properties:
  *
  * Integers in each row are sorted in ascending from left to right.
  * Integers in each column are sorted in ascending from top to bottom.
  * **/

def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
  matrix.flatMap(_.filter(_ == target)).headOption match {
    case Some(x) => true
    case _ => false
  }
}

searchMatrix(
    Array(
    Array(1, 4, 7, 11, 15),
    Array(2, 5, 8, 12, 19),
    Array(3, 6, 9, 16, 22),
    Array(10, 13, 14, 17, 24),
    Array(18, 21, 23, 26, 30)),
  5)

/**********************************************************************/