package com.scalaprogramming.competitive

object JumpGame {

  /**
  * This Solution can be made better and faster..
  */
  def canJump(nums: Array[Int]): Boolean = {

    // To avoid re-computation of already visited index.
    val visited = scala.collection.mutable.Set[Int]()

    if (nums.isEmpty) false
    else if (nums.size == 1) true // If size of 1 than already at the last index.
    else {
      // Recursive functions to eagerly check the farthest distance achieved.
      def _doJump(startIdx: Int): Boolean = {
        // case 1: If input index is greater than or equal at the length of array
        // Case 2: If the jump available at index is greater than the array length.
        if (startIdx >= nums.length - 1 || nums(startIdx) >= nums.length - 1) true
        else if (!visited.contains(startIdx)) {
          // We are exploring this index now, lets add it to the visited Set.
          visited += startIdx

          // First check, based on the jump at the input index, what is the maximum number of options that can be availed
          // e.g, if at Arr(startIdx) we get 5, than we can go 1 or 2, or 3... upto maximum 5 jumps.
          // We are now going to check based on jump chosen, can we reach the end of array..
          val avlJumpOptions = nums.slice(startIdx + 1, startIdx + nums(startIdx) + 1)
          val Output = for (idx <- avlJumpOptions.length - 1 to 0 by -1) yield { // explore from the max possible to min possible jump.
            val offset = startIdx + idx + 1 // avlJumpOptions is 0 based index, hence add 1 to find the offset for original array.
            val maxDistanceAtOffset = offset + avlJumpOptions(idx)
            // at the new offset, using the jump options, can we reach the end of Array?
            if (maxDistanceAtOffset >= nums.length - 1) true // if yes, we are done.
            else {
              _doJump(offset) // else, explore the new offset recursively.
            }
          }

          Output.contains(true) // if we have found a true in any of our recursive call, than array end is reachable.
        } else false // if the start index is visited, skip this index...
      }

      // Explore starting 0 index.
      _doJump(0)
    }
  }


  def main(args: Array[String]): Unit = {
    println(canJump(Array()))
    println(canJump(Array(0)))
    println(canJump(Array(1)))
    println(canJump(Array(2, 0)))
    println(canJump(Array(2, 5, 0, 0)))
    println(canJump(Array(2, 3, 1, 1, 4)))
    println(canJump(Array(3, 2, 1, 0, 4)))
    println(canJump(Array(1, 1, 1, 0)))
    println(canJump(Array(1, 1, 1, 1)))
    println(canJump(Array(1, 2, 0, 1)))
    println(canJump(Array(1, 1, 2, 2, 0, 1, 1)))
    println(canJump(Array(2, 0, 0)))
    println(canJump(Array(5, 9, 3, 2, 1, 0, 2, 3, 3, 1, 0, 0)))
    println(canJump(Array(4, 2, 0, 0, 1, 1, 4, 4, 4, 0, 4, 0)))
    println(canJump(Array(4, 0, 4, 2, 2, 0, 1, 3, 3, 0, 3)))
    println(canJump(Array(5, 4, 0, 2, 0, 1, 0, 1, 0)))
    println(canJump(Array(1, 2, 2, 6, 3, 6, 1, 8, 9, 4, 7, 6, 5, 6, 8, 2, 6, 1, 3, 6, 6, 6, 3, 2, 4, 9, 4, 5, 9, 8,
      2, 2, 1, 6, 1, 6, 2, 2, 6, 1, 8, 6, 8, 3, 2, 8, 5, 8, 0, 1, 4, 8, 7, 9, 0, 3, 9, 4, 8, 0, 2, 2, 5, 5, 8, 6, 3,
      1, 0, 2, 4, 9, 8, 4, 4, 2, 3, 2, 2, 5, 5, 9, 3, 2, 8, 5, 8, 9, 1, 6, 2, 5, 9, 9, 3, 9, 7, 6, 0, 7, 8, 7, 8, 8,
      3, 5, 0))
    )
  }
}
