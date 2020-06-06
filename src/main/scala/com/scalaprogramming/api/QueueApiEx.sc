import scala.collection.mutable

/**
  * Priority Queue Api Ex
  */

val pq = new mutable.PriorityQueue[Int]()

// Add all element of an IterableOnce to a Priority Queue
pq ++ Seq(3, 4, 2)

//Alias for Add All
pq ++= Seq(10, -3, 3)

// Alias for addOne
pq += 5

pq.addAll(List(4, 6, 7, 1, 2))

pq.addOne(100)

pq.addString(new StringBuilder())
pq.addString(new StringBuilder(), ", ")
pq.addString(new StringBuilder(), "[ ", ", ", " ]")

val cpq = pq.clone()

// Collect result of applying the partial function to each element.
cpq.collect(_ > 2)

//Collect 1st element to which the partial function is applied.
cpq.collectFirst(_ > 2)

//Concat a collection.
cpq.concat(List(111, 112))

// Test if every element of this(cpq) collection corresponds to every element
// of that(pq) collection and the relation is defined by the test predicate.
cpq.corresponds(pq)((a, b) => a == b)

// Count the number of element which satisfy the test predicate.
cpq.count(_ > 0)

// Dequeue the element with the highest priority
cpq.dequeue()

//cpq.groupBy(identity)(_ % 2 == 0)













println(cpq.mkString(", "))



