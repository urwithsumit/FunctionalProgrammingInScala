/**
  * P46 (**) Truth tables for logical expressions.
  * Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence) which return true or false according to the result of their respective operations; e.g. and(A, B) is true if and only if both A and B are true.
  * scala> and(true, true)
  * res0: Boolean = true
  *
  * scala> xor(true. true)
  * res1: Boolean = false
  * A logical expression in two variables can then be written as an function of two variables, e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))
  *
  * Now, write a function called table2 which prints the truth table of a given logical expression in two variables.
  *
  * scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
  * A     B     result
  * true  true  true
  * true  false true
  * false true  false
  * false false false
  *
  */
def and(a: Boolean, b: Boolean) = (a,b) match {
  case (true, true) => true
  case _ => false
}

def or(a: Boolean, b: Boolean) = (a, b) match {
  case (false, false) => false
  case _ => true
}

def not(a: Boolean) = a match {
  case true => false
  case _ => true
}

def nor(a: Boolean, b: Boolean) = not(or(a,b))
def nand(a: Boolean, b: Boolean) = not(and(a,b))

def equ(a: Boolean, b: Boolean) = or(and(a,b), and(not(a), not(b))) //
def xor(a:Boolean, b: Boolean) = not(equ(a,b))

def table2(f:(Boolean, Boolean) => Boolean ) = {
  val TT = List(true, false)
  TT map(x => TT map (y => println(s"${x}  ${y} = ${f(x, y)}")))
}


table2((a: Boolean, b: Boolean) => and(a, or(a, b)))