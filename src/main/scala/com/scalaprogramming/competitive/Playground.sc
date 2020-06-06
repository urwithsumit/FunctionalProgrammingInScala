def hasGroupsSizeX(deck: Array[Int]): Boolean = {

  def gcd(x1: Int, x2: Int): Int = {
    val num = if (x1 > x2) x1 else x2
    val den = if (x1 == num) x2 else x1

    if (den == 0) num
    else gcd(den, num % den)
  }

  if (deck.size < 2) false
  else {
    val reduced = deck.groupMapReduce(c => c)(c => 1)(_ + _)
    val freq = reduced.values.toArray.distinct.sortInPlace
    println(freq.mkString(", "))
    if (freq.size > 1) {
      val size = gcd(freq(1), freq(0))
      if (size > 1) { // We have to make Groups >= 2
        freq.map(_ % size == 0).contains(false) == false
      } else false
    } else true
  }
}

def maxSubarraySumCircular(A: Array[Int]): Int = {

  def _maxKadanes(nums: Array[Int]): Int = {
    nums.tail.scanLeft(nums.head)((r, c) => math.max(r + c, c)).max
  }

  if (A.forall(_ < 0)) A.max
  else {
    val _invA = A.map(_ * -1)
    val _positiveKad = _maxKadanes(A)
    val _negKadane = _maxKadanes(_invA)

    val wrapSum = A.sum + _negKadane
    math.max(wrapSum, _positiveKad)
  }
}


maxSubarraySumCircular(Array(0, 5, 8, -9, 9, -7, 3, -2))
maxSubarraySumCircular(Array(2, -2, 2, 7, 8, 0))
maxSubarraySumCircular(Array(1, -2, 3, -2))
maxSubarraySumCircular(Array(5, -3, 5))
maxSubarraySumCircular(Array(5))
maxSubarraySumCircular(Array(-5))
maxSubarraySumCircular(Array(3, -1, 2, -1))
maxSubarraySumCircular(Array(3, -2, 2, -3))
maxSubarraySumCircular(Array(-2, -3, -1))


////////
// carabeqiitefw  k = 5, out: eqiit
// cabw k = 4, cabw
// ptfw k = 2, not found

def maxVowels(str: String, k: Int): String = {
  val vowels = Array('a', 'e', 'i', 'o', 'u')

  def countVowels(str: String): Int = {
    str.foldLeft(0: Int) {
      case (sum, ch) => if (vowels.contains(ch)) (sum + 1) else sum
    }
  }

  var result = "Not Found"

  if (str.length <= k && countVowels(str) > 0) result = str
  else {
    var left = 0
    var right = k
    var count = 0
    while (right < str.length) {
      val _count = countVowels(str.substring(left, right))
      if (_count > 0 && _count > count) {
        count = _count
        result = str.substring(left, right)
      }
      left += 1
      right += 1
    }
  }

  result
}

maxVowels("carbeqiitefw", 5)
maxVowels("cabw", 4)
maxVowels("ptfw", 2)

"str".permutations

def maxUncrossedLines(A: Array[Int], B: Array[Int]): Int = {
  val (aLength, bLength) = (A.length, B.length)
  val maxLinesCountArray = Array.ofDim[Int](aLength + 1, bLength + 1)

  for (i <- 1 to aLength; j <- 1 to bLength) {
    maxLinesCountArray(i)(j) =  if (A(i - 1) == B(j - 1))
      maxLinesCountArray(i - 1)(j - 1) + 1
    else  Math.max(maxLinesCountArray(i - 1)(j), maxLinesCountArray(i)(j - 1))
  }

  println(maxLinesCountArray.map(x => x.mkString(("\t"))).mkString("\n"))
  maxLinesCountArray.last.last
}

maxUncrossedLines(
  Array(2,3,4,1,3,3,2,4,2,2,1,5,2,4,3,4,4,5,1,5,1,5,4,3,1,2,5,2,4,4),
  Array(2,2,4,2,4,1,1,5,5,3,2,1,1,1,3,1,2,5,2,4,3,4,5,5,3,3,5,1,4,3)
)

val s = "abbbbbbba"
def isPalindrome(i: Int, j: Int): Boolean = {
  var l = i
  var r = j

  while(s.charAt(l) == s.charAt(r)) {
    l += 1
    r -= 1
  }

  r - l <= 1
}

isPalindrome(0,4)