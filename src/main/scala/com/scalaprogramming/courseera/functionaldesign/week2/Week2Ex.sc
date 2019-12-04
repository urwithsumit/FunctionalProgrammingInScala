
//Stream is deprecated, So will do the Stream example using LazyList

//Ex with Lazy List and cons
def streamRange(lo: Int, hi: Int): LazyList[Int] = {
  if(lo >= hi) LazyList.empty
  else LazyList.cons(lo, streamRange(lo + 1, hi))
}

// Ex with #:: instead of cons.
def streamRange2(lo: Int, hi: Int): LazyList[Int] = {
  if(lo >= hi) LazyList.empty
  else lo #:: streamRange2(lo + 1, hi)
}

// Ex with List. No lazy evaluation happens here.
def listRange(lo: Int, hi: Int): List[Int] = {
  if(lo >= hi) Nil
  else lo :: listRange(lo + 1, hi)
}

streamRange(1, 10).headOption
streamRange(1, 10).lastOption
streamRange(1, 10)
streamRange(1, 10).take(3) // Prints nothing
streamRange(1, 10).take(3).toList // Calling .toList, forces evaluation of the Lazy List

streamRange2(1, 10).headOption
streamRange2(1, 10).lastOption
streamRange2(1, 10)
streamRange2(1, 10).take(3) // Prints nothing
streamRange2(1, 10).take(3).toList // Calling .toList, forces evaluation of the Lazy List


listRange(1, 10)