package com.practice.functional.prgm.scala

/**
  * P47 (*) Truth tables for logical expressions (2).
  * Continue problem P46 by redefining and, or, etc as operators.
  * (i.e. make them methods of a new class with an implicit conversion from Boolean.)
  * not will have to be left as a object method.
  * scala> table2((a: Boolean, b: Boolean) => a and (a or not(b)))
  * A     B     result
  * true  true  true
  * true  false true
  * false true  false
  * false false false
  */

object S99LogicalOp {

  implicit def convert(a: Boolean) = new S99LogicalOp(a)

  def not(a: Boolean) = a match {
    case true => false
    case _ => true
  }
}

class S99LogicalOp(a: Boolean) {

  import S99LogicalOp._

  def nor(b: Boolean) = not(a or b)

  def nand(b: Boolean) = not(a and b)

  def and(b: Boolean) = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def xor(b: Boolean) = not(a equ b)

  def equ(b: Boolean) = ((a and b) or (not(a) and not(b)))

  def or(b: Boolean) = (a, b) match {
    case (false, false) => false
    case _ => true
  }
}

class Test extends App {


}

