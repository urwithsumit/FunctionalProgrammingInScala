val ls = List(1, 2, 3)

// ++ Requires a traversable type to add to list.
ls ++ Seq(4, 5)
ls ++ List(5, 6)
ls ++ Set(5, 6)

// ++: Prepend All
Seq(2, 3) ++: ls

// +: prepend element
3 +: ls

// :+ append element
ls :+ 3

// :: add element at the begin of list
9 :: ls

// ::: add  element of a given list at the start of this list
List(9, 8, 7) ::: ls

//addString
val b = new StringBuilder
val h = ls.addString(b)

val b2 = new StringBuilder
val h2 = ls.addString(b2, "List(", ", ", ")")


val b3 = new StringBuilder
val h3 = ls.addString(b3, ",")

// andThen
val PartialEx = ls filter (_ > 2) andThen (_ + 3)

// appended
val Ex = ls appended (9)

val Ex2 = ls appended (List(8, 9, 10))

//Append All
val Ex3 = ls appendedAll (List(8, 9, 10))

//apply
ls.apply(2)

//ls.applyOrElse(4, PartialEx)

// Collect
val divide: PartialFunction[Int, Int] = {
  case d: Int if d != 0 => 42 / d
}

List(0, 1, 2) collect {
  divide
}

val myFunc: PartialFunction[Any, Any] = {
  case c: Int => c + 3
  case s: String => s"Scala developer: ${s}"
}

List(0, 1, 2, "Sumit") collect {
  myFunc
}

// CollectFirst
List(0, 1, 2) collectFirst {
  divide
}

List(0, 1, 2, "Sumit") collectFirst {
  myFunc
}

//combinations. Returns iterator. creates combination of given length.
"Sumit".toSeq.combinations(4).foreach(println)

//Compose
val divideby2 = (x: Int) => {
  print(s"Input for Divideby2: $x. ");
  x / 2
}
val Multby3 = (x: Int) => {
  print(s"Input for Multby3: $x. ");
  x * 3
}

// Composes another partial function k(i.e. Multby3) with this (i.e. divideby2) partial
// function so that this partial function gets applied to results of k.
// f(g(x)) where g(x) is k and f(k) is this
val DivideBy2AndMultBy3 = divideby2 compose Multby3 // Here Mult3 result is the input for divide
val _DivideBy2AndMultBy3 = divideby2 andThen Multby3 // Here divideby2 is executed first and its result is input for multby3

DivideBy2AndMultBy3(8)
_DivideBy2AndMultBy3(10)

val ConcatEx = "Sumit".toList concat "Kumar".toList

ConcatEx.contains('s')
ConcatEx.contains('K')

ConcatEx.containsSlice("uma".toList)

val arr: Array[Char] = Array.ofDim(10)
ConcatEx.copyToArray(arr)
println(arr.mkString(","))

ConcatEx.copyToArray(arr, 3)
println(arr.mkString(","))

val ls2 = List(2, 4, 6)

// Correspond: Tests whether every element of this collection's iterator
// relates to the corresponding element of another collection
// by satisfying a test predicate.
val CorrespondEx = ls2.corresponds(ls)(_ == _ * 2)

val DiffEx = ls2.diff(ls)

//Not clear with Distinct by
val DistinctBy = List(1, 3, 5, 2, 6) distinctBy (_ % 2 == 0)

// Head of collection match the condition, it is dropped.
// next element is 5 and does not match condition. Dropwhile stops
// when condition do not meet.
val DropWhile = List(3, 5, 6) dropWhile (_ % 3 == 0)


val firstChar: String => Option[Char] = _.headOption

List(1, 3, 5, 2, 6) endsWith (Seq(5, 2, 6))

List(1, 2, 3, 4).exists(_ > 3)

List(1, 2, 3, 4, 5) filter (_ > 3)

List(1, 2, 3, 4, 5) filterNot (_ > 3)

List(1, 2, 3, 4, 5) find (_ > 3)

List(1, 2, 3, 4, 5) findLast (_ > 3)

List(1, 2, 3, 4, 5) forall (_ > 3)

List(1, 2, 3, 4, 5) forall (_ > 0)

List(1, 2, 3, 4, 5) foreach print

List(1, 2, 3, 4, 5, 6) groupBy (_ % 2 == 0)

List(List(12, 3), List("a", "b", "c"), Map(1 -> 2, 3 -> 4)).flatten

//GroupMap

//GroupMapReduce

ls.hashCode()

ls.head

ls.headOption

List(2, 3, 5, 3, 4, 6, 7, 9).indexOf(3)

List(2, 3, 5, 3, 4, 6, 7, 9, 3).indexOf(3, 2)

List(2, 3, 5, 3, 4, 6, 7, 9, 3).indexOfSlice(Seq(4, 6))

List(2, 3, 5, 3, 4, 6, 7, 9, 3).indexOfSlice(Seq(4, 6), 2) // index after 2

List(2, 3, 5, 3, 4, 6, 7, 9, 3).indexWhere(_ - 3 == 4)

List(2, 3, 5, 3, 4, 6, 7, 9, 3, 7).indexWhere(_ - 3 == 4, 7)

List(2, 3, 5, 3, 4, 6, 7, 9, 3, 7).indices

List(2, 3, 5, 3, 4, 6, 7, 9, 3, 7).init

//Not clear with inits
List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).inits.foreach(print)

List(2, 3, 5, 3, 4, 6, 7, 9, 3, 7).intersect(List(3, 4, 5, 2, 33, 4, 1, 22, 3, 55, 66, 0))

ls.isDefinedAt(10)

ls.isTraversableAgain

ls.iterator

ls.lastIndexOf(3)

List(2, 3, 5, 3, 4, 6, 7, 9, 3, 7).lastIndexOfSlice(List(7, 9))

List(2, 3, 5, 3, 4, 6, 7, 9, 3).lastIndexOfSlice(Seq(4, 6))

List(2, 3, 5, 3, 4, 6, 7, 9, 3).lastIndexOfSlice(Seq(4, 6), 2) // find before index 2

List(2, 3, 5, 3, 4, 6, 7, 9, 3).lastIndexWhere(_ - 3 == 4)

List(2, 3, 5, 3, 4, 6, 7, 9, 3, 7).lastIndexWhere(_ - 3 == 4, 7)

ls.last

ls.lastOption

val LzyZip = List(2, 3, 5, 3, 4, 6, 7, 9, 3, 7).lazyZip(List("a","b","c","d","e","f","g","h","i","j","k","l"))
LzyZip.mkString(",")

List(2, 3, 5, 3, 4, 6, 7, 9, 3, 7).lengthCompare(List("a","b","c","d","e","f","g","h","i","j"))

import scala.util.chaining._

val times6 = (_: Int) * 6
val i = (1 - 2 - 3).pipe(times6).pipe(scala.math.abs)

val xs = List(1, 2, 3).tap(ys => println("debug " + ys.toString))