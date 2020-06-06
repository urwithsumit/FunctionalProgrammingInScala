import scala.collection.mutable.ListBuffer

/**
* Replace Elements with Greatest Element on Right Side
*/
def replaceElements(arr: Array[Int]): Array[Int] = {
  if (!arr.isEmpty) {
    arr.indices.map { idx =>
      arr(idx) = if (idx + 1 < arr.length)
        arr.slice(idx + 1, arr.length).max
      else -1
    }
  }
  arr
}

replaceElements(Array(17, 18, 5, 4, 6, 1))

/**********************************/

/**
* Remove Duplicates from Sorted Array
* 2,2,2,3,3,4,4,5,5,6,6,6
*/
def removeDuplicates(nums: Array[Int]): Int = {
  if (!nums.isEmpty && nums.length > 1) {
    var j = 0
    for (i <- 1 until nums.length) {
      if (nums(i) != nums(j)) {
        j += 1
        nums(j) = nums(i)
      }
    }
    j + 1
  } else nums.length
}

removeDuplicates(Array(2, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6))
removeDuplicates(Array())
removeDuplicates(Array(1, 2, 3, 4))
removeDuplicates(Array(1))

/**********************************/

/**
* Move Zeroes to end of Array
*/
def moveZeroes(nums: Array[Int]): Unit = {

  def moveDigitK(K: Int) = {
    if (!nums.isEmpty) {
      var j = 0
      for (i <- 0 until nums.length) {
        if (nums(i) != K) {
          if (j >= 0) {
            val tmp = nums(j)
            nums(j) = nums(i)
            nums(i) = tmp
            j += 1
          }
        }
      }
    }
  }

  moveDigitK(0)
}

moveZeroes(Array(0, 1, 0, 0, 4, 0, 3, 12))

/**********************************/

/**
* Sort Array By Parity
*
*/
def sortArrayByParity(nums: Array[Int]): Array[Int] = {

  def evenOdd(K: Int): Boolean = K % 2 == 0

  def moveDigitK(f: Int => Boolean): Unit = {
    if (!nums.isEmpty) {
      var j = 0
      for (i <- 0 until nums.length) {
        if (f(nums(i))) {
          if (j >= 0) {
            val tmp = nums(j)
            nums(j) = nums(i)
            nums(i) = tmp
            j += 1
          }
        }
      }
    }
  }

  moveDigitK(evenOdd)

  nums
}

println(sortArrayByParity(Array(3, 1, 2, 4)).mkString(", "))

/**********************************/

/**
* Check If N and Its Double Exist
*/
def checkIfExist(arr: Array[Int]): Boolean = {

  val dict = scala.collection.mutable.HashMap[Int, Int]()

  val ans = for (item <- 0 until arr.length) yield {
    if (dict.contains(2 * arr(item)) || (arr(item) % 2 == 0 && dict.contains(arr(item) / 2))) {
      true
    } else {
      dict.addOne(arr(item) -> item)
      false
    }
  }

  ans.exists(_ == true)

}

checkIfExist(Array(-4, 0, 10, -19, -2, 6, -8))
checkIfExist(Array(0, 0))
checkIfExist(Array(0))

/**********************************/

def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
  if (!nums.isEmpty) {
    nums.scanLeft(0)(
      (r, c) => if (c == 0) c else r + c
    ).max
  } else 0
}


findMaxConsecutiveOnes(Array(1, 0, 0, 1, 0, 1, 0, 1))

/*****************************************/

def findNumbers(nums: Array[Int]): Int = {

  def digits(i: Int, count: Int = 0): Int = {
    if (i > 0) digits(i / 10, count + 1)
    else count
  }

  nums.map(x => digits(x)).count(_ % 2 == 0)

}

findNumbers(Array(12, 345, 2, 6, 789))

/*****************************************/

def duplicateZeros(arr: Array[Int]): Unit = {

  var i = 0

  while (i < arr.length) {
    if (arr(i) == 0 && i + 1 < arr.length) {
      i += 1
      // Save the next value for shifting
      var shift = arr(i)
      // Now set the next value to 0
      arr(i) = 0
      // Set the j counter to the next non-duplicated value
      var j = i + 1
      //Shift all values
      while (j < arr.length) {
        val tmp = arr(j)
        arr(j) = shift
        shift = tmp
        j += 1
      }
    }
    i += 1
  }
}

duplicateZeros(Array(1, 0, 2, 3, 0, 4, 5, 0))

/************************************************/

/**
* Valid Mountain Array
*/
def validMountainArray(A: Array[Int]): Boolean = {

  if (A.length < 3) return false

  var i = 0

  // Going Up Hill,
  while (i + 1 < A.length && A(i) < A(i + 1)) {
    i += 1
  }

  if (i == 0 || i == A.length - 1) return false

  // Going Downhill, if exist
  while (i + 1 < A.length && A(i) > A(i + 1)) {
    i += 1
  }

  // Did reach downhill completely?
  i == A.length - 1
}

println(validMountainArray(Array(0, 3, 2, 1)))
println(validMountainArray(Array(2, 1)))
println(validMountainArray(Array(3, 5, 5)))
println(validMountainArray(Array(0, 2, 3, 4, 5, 2, 1, 0)))
println(validMountainArray(Array(0, 2, 3, 3, 5, 2, 1, 0)))
println(validMountainArray(Array()))
println(validMountainArray(Array(9)))
println(validMountainArray(Array(1, 3, 2)))


/************************************************/

/**
* Merge Sorted List. longArr has sufficient space to fit the shortArr.
*/
def merge(longArr: Array[Int], m: Int, shortArr: Array[Int], n: Int): Unit = {

  if (m > 0 && n > 0) {
    var i = 0
    var j = 0
    while (i < m && j < n) {
      if (longArr(i) > shortArr(j)) {
        val tmp = shortArr(j)
        shortArr(j) = longArr(i)
        longArr(i) = tmp

        shortArr.sortInPlace()
      }
      i += 1
    }
  }

  shortArr.copyToArray(longArr, m)
}

merge(
  Array(1, 7, 8, 0, 0, 0), 3,
  Array(2, 5, 6), 3)

merge(Array(1, 2, 3, 0, 0, 0), 3, Array(-6, -7, -6), 3)

merge(Array(0), 0, Array(1), 1)

merge(
  Array(1, 7, 8, 10, 12, 0, 0, 0), 5,
  Array(2, 5, 6), 3)

merge(
  Array(1, 2, 3, 0, 0, 0), 3,
  Array(2, 5, 6), 3)


/************************************************/
//Find Pivot Index

def pivotIndex(nums: Array[Int]): Int = {

  if (nums.length < 1) return -1

  val buffer = scala.collection.mutable.ListBuffer[Int]()
  val TotalSum = nums.sum

  var sum = 0

  for (i <- 0 until nums.length) {
    if ((TotalSum - sum - nums(i)) == sum) {
      buffer.addOne(i)
    } else sum += nums(i)
  }

  // Problems ask to return left most index, which will at the head of buffer.
  if (buffer.isEmpty) -1 else buffer.head
}

pivotIndex(Array(1, 7, 3, 6, 5, 6))
pivotIndex(Array(1, 2, 3))
pivotIndex(Array(-1, -1, -1, -1, -1, -1))
pivotIndex(Array(-1, -1, -1, -1, -1, 0))
pivotIndex(Array(-1, -1, 0, 0, -1, -1))


////////////////////////////////////////////

def dominantIndex(nums: Array[Int]): Int = {

  

}