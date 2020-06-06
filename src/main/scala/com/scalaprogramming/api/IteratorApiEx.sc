// Checks if the 2 iterators are identical
val x = List(1, 2, 3, 4, 5).iterator.sameElements(List(1, 2, 5, 3, 4))

val y = List(1, 2, 3, 4, 5).iterator.sameElements(List(1, 2, 3, 4, 5))

//Create a Collection with the specified elements
Iterator.apply(Array(1, 2, 3, 4))

Iterator.continually(4).take(4).toSeq

Iterator.fill(4)(3).toSeq

Iterator.iterate(4)(_ * 2).take(5).toSeq

Iterator.single(3).toList