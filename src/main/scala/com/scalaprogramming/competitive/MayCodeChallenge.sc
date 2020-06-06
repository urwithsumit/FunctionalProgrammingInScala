2147395599 / 2 * 2147395599 / 2 > 0

def isBadVersion(version: Int): Boolean = {
  if (version >= 4) true else false
}


def firstBadVersion(n: Int): Int = {

  def doBinarySearch(range: Range): Int = {
    if (range.head == range.last) range.head // When only single element in range.
    else {
      // The below approach of finding mid will avoid overflows
      val mid = range.head + (range.last - range.head) / 2
      if (isBadVersion(mid)) {
        // Bad version is still true, so lets search the lower half of range
        doBinarySearch(range.head to mid)
      } else {
        // Bad version is now false, so lets reduce search by searching upper half.
        doBinarySearch(mid + 1 to range.last)
      }
    }
  }

  doBinarySearch(1 to n)
}

firstBadVersion(6)

///////////////////////////////////////////////////
/**
* https://leetcode.com/explore/challenge/card/may-leetcoding-challenge/534/week-1-may-1st-may-7th/3319/
*/
def findComplement(num: Int): Int = {

  val _bits = num.toBinaryString
  val (ans, idx) = _bits.foldLeft(0: Int, 1: Int) {
    case ((ans, idx), ch) =>
      if (ch == '1') (ans, idx + 1) // 1 wil be flipped to 0, so ignore it.
      else (ans + math.pow(2, _bits.length - idx).toInt, idx + 1)
  }

  ans
}

findComplement(5)

object Solution {

  def findClosestElements(arr: Array[Int], k: Int, x: Int): List[Int] = {

    if (arr.isEmpty || arr.length < 2) arr.toList
    else if (x > arr.last) arr.takeRight(k).toList
    else if (x < arr.head) arr.take(k).toList
    else {

      import scala.collection.mutable.Set

      def _find(lo: Int, hi: Int, visited: Set[(Int, Int)] = Set() ): Int = {
        val mid = lo + (hi - lo) / 2

        if (visited.contains((lo, hi))) return hi
        else visited.addOne((lo, hi))

        if (lo <= hi) {
          if (arr(mid) == x) return mid
          else if (arr(mid) > x) return _find(lo, mid, visited)
          else return _find(mid, hi, visited)
        }

        mid
      }

      val idx = _find(0, arr.length - 1)

      // Post processing
      //Trick or Say Tip: Selection range will be in [idx - k - 1, idx + k - 1]
      var low = math.max(0, idx - k - 1)
      var high = math.min(arr.size - 1, idx + k - 1)

      while (high - low > k - 1) {
        if (low < 0 || (x - arr(low) <= arr(high) - x)) high -= 1
        else if (high > arr.size - 1 || (x - arr(low) > arr(high) - x)) low += 1
        else {
          return arr.slice(low, high + 1).take(k).toList
        }
      }

      arr.slice(low, high + 1).take(k).toList
    }
  }
}

