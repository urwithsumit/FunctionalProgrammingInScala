package com.scalaprogramming.courseera.functionaldesign.week2

/**
  * Lazy Evaluation
  */
object Lecture3_2 extends App{
  def expr = {
    // val gets evaluate immediately.
    // Subsequent evaluation do not occur
    val x = {
      print("x")
      1
    }

    // lazy Val not evaluated until invocation.
    // Once invoked, its not evaluated on teh subsequent invocation.
    // Initial evaluated value is used on subsequent invocation.
    lazy val y = {
      print("y")
      2
    }

    // def not evaluated until invocation.
    // It is evaluated for every invocation.
    def z = {
      print("z")
      3
    }

    z + y + x + z + y + x

  }

  expr
}
