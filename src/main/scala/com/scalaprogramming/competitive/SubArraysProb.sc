
/**
* https://leetcode.com/problems/subarray-sums-divisible-by-k/
*/
def subarraysDivByK(A: Array[Int], K: Int): Int = {

  // Prefix Map of [(((sum % K) + K) % K) -> count]
  A.scanLeft(0)(_ + _).foldLeft(Map[Int, Int](), 0: Int) {
    case ((prefix, occurrence), sum) => {
      //% is remainder operator in java & scala (and not a proper modulus).
      // To get a positive number, we have to add by the base: a mod b = ((a % b) + b) % b.
      val sumModK = (sum % K + K) % K

      if (prefix.contains(sumModK)) {
        (prefix + (sumModK -> (prefix.getOrElse(sumModK, 0) + 1)), occurrence + prefix.getOrElse(sumModK, 0))
      } else (prefix + (sumModK -> (prefix.getOrElse(sumModK, 0) + 1)), occurrence)
    }
  }._2
}

subarraysDivByK(Array(4, 5, 0, -2, -3, 1), 5)
subarraysDivByK(Array(-1, 2, 9), 2) // 0, -1, 1, 10
subarraysDivByK(Array(2, -2, 2, -4), 6)

/**************************************************************************************************/

/**
* https://leetcode.com/problems/binary-subarrays-with-sum/
*/
def numSubarraysWithSum(A: Array[Int], S: Int): Int = {

  // Prefix Map of [sum -> count]
  val ans = A.scanLeft(0)(_ + _).foldLeft(Map[Int, Int](), 0: Int) {
    case ((prefix, occurrence), sum) => {
      if (prefix.contains(sum - S)) {
        (prefix + (sum -> (prefix.getOrElse(sum, 0) + 1)), occurrence + prefix.getOrElse(sum - S, 0)) // map value default to 0
      } else (prefix + (sum -> (prefix.getOrElse(sum, 0) + 1)), occurrence) // map value default to 1
    }
  }

  println(ans._1.mkString(", "))
  ans._2
}

numSubarraysWithSum(Array(1, 0, 1, 0, 1), 2) //0 1 1 2 2 3
numSubarraysWithSum(Array(0, 0, 0, 0, 0), 2) //0 1 1 2 2 3
numSubarraysWithSum(Array(1, 1, 1, 1, 1), 2) //0 1 1 2 2 3

/**************************************************************************************************/


/**
* Subarray Sum Equals K
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

/**************************************************************************************************/

/**
* https://leetcode.com/explore/challenge/card/30-day-leetcoding-challenge/529/week-2/3298/
*
* Given a binary array, find the maximum length of a contiguous subarray with equal number of 0 and 1
*/
def findMaxLength(nums: Array[Int]): Int = {

  nums.indices.map(x => if (nums(x) == 0) nums(x) = -1)

  val preTotal = nums.scanLeft(0)(_ + _)

  // [(prefix_sum -> index)]
  preTotal.indices.foldLeft(Map[Int, Int](), 0: Int) {
    case ((acc, maxLen), idx) =>
      if (acc.contains(preTotal(idx))) {
        (acc, math.max(maxLen, idx - acc.getOrElse(preTotal(idx), 0)))
      } else (acc + (preTotal(idx) -> idx), maxLen)
  }._2

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

/**************************************************************************************************/
