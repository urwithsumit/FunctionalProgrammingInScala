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
