package com.scalaprogramming.competitive


/**
  *
  * https://leetcode.com/problems/two-sum/
  *
  * Submission Status: Success.
  *
  */

object TwoSum {

  // Brute Force approach, failed with Timeout for large input.
  def twoSum_BruteForce(nums: Array[Int], target: Int): Array[Int] = {

    val Result = for {
      i <- 0 until nums.length
      k <- i + 1 until nums.length // Start with i + 1
    } yield {
      if (nums(i) + nums(k) == target) {
        (i, k)
      } else (-1, -1)
    }

    val Ans = (Result.distinct.unzip._1 ++ Result.distinct.unzip._2) filterNot (_ == -1)

    Ans.toArray.distinct

  }

  def main(args: Array[String]) = {
    val nums = Array(3, 2, 4)

    println(twoSum_Optimal(nums, 6).mkString(","))
  }

  // Optimal Approach: Passed all Test Cases.
  def twoSum_Optimal(nums: Array[Int], target: Int): Array[Int] = {

    val Zipped: Map[Int, Int] = nums.zipWithIndex.toMap

    val Result = for {
      i <- 0 until nums.length
    } yield {
      val value = Zipped.getOrElse(target - nums(i), -1) // O(1)
      // Check index is not -1 and return value is not same as index i.
      if (value != -1 && i != value) (i, value) else (-1, -1)
    }

    val Ans = (Result.distinct.map(_._1) ++ Result.distinct.map(_._2)) filterNot (_ == -1)

    Ans.toArray.distinct

  }


}
