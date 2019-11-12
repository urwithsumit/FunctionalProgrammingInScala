package com.practice.functional.prgm.scala

/**
 * This Example is from Scala 2.13.0 API
 * LazyList is a new datastructure to replace Streams in the new version
 */
object LazyFibonacci extends App {

  lazy val fibs: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map(n => n._1 + n._2)
  fibs.take(10).foreach(println)


  lazy val fib: LazyList[Int] = {
    def loop(h: Int, n: Int): LazyList[Int] = h #:: loop(n, h + n)

    loop(0, 1)
  }
  fib.take(10).foreach(println)
}
