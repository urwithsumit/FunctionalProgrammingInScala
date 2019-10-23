package com.practice.functional.prgm.scala



object OrderskyPatternMatch extends App {

  /**
    * Sealed classes helps to ensure all possible cases are covered for pattern matching
    *
    */
  sealed class Expr
  case class Var(name: String) extends Expr
  case class Number(num: Double) extends Expr
  case class UnOps(operator: String, arg: Expr) extends Expr
  case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

  import Math.{E,PI}

  // Variable Pattern Matching. Here E matches to a variable case class "must".
  // E gets assigned to must.
  val Ans1 = E match {
    case must => s"Variable PAttern match ex ${must}"
    case _ => "No match"
  }

  println(Ans1)


  // Here PI is identified as a Constant, and hence not a variable pattern matching.
  //

  val Ans2 = E match {
    case PI => s"Variable PAttern match ex ${PI}"
    case _ => "No match"
  }

  println(Ans2)


  List(1,2,3,4,5) match {
    case Seq(1, c @ _*) => println(c)
    case _ =>
  }

  /**
    * Using @unchecked will suppress exhaustive match check validation.
    * See that in this example there is no case _ .
    * @param list
    */
  def test(list: Array[Int]) =
  (list: @unchecked) match {
    case Array(1, c @ _*) => println(c)
  }

  test(Array(1))



}
