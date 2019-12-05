package com.scalaprogramming.courseera.functionaldesign.week2

object Lecture3_3 extends App {

  // The from method can creates an infinite sequence, and starting with the initial value of n.
  def from(n: Int): LazyList[Int] = n #:: from(n + 1)

  val nats = from(0)

  val m4s = nats map (_ * 4)

  println((m4s take 20).toList)

  /**
    * Sieves method for finding prime, take the head of list and than remove its multiple from the tail of the list.
    * This repeats.
    *
    * @param s
    * @return
    */
  def sieve(s: LazyList[Int]): LazyList[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))

  println((sieve(from(2)) take 200).toList)

}
