val ls = List(1,2,3)

// ++ Requires a traversable type to add to list.
ls ++ Seq(4,5)
ls ++ List(5,6)
ls ++ Set(5,6)

// ++: Prepend All
Seq(2,3) ++: ls

// +: prepend element
3 +: ls

// :+ append element
ls :+ 3

// :: add element at the begin of list
9 :: ls

// ::: add  element of a given list at the start of this list
List(9,8,7) ::: ls

//addString
val b = new StringBuilder
val h = ls.addString(b)

val b2 = new StringBuilder
val h2 = ls.addString(b2, "List(", ", ", ")")


val b3 = new StringBuilder
val h3 = ls.addString(b3, ",")

val divide: PartialFunction[Int, Int] = {
  case d: Int if d != 0 => 42 / d
}

List(0,1,2) collect { divide }

val myFunc: PartialFunction[Any, Any] = {
  case c: Int => c + 3
  case s: String => s"Scala developer: ${s}"
}

List(0,1,2, "Sumit") collect { myFunc }