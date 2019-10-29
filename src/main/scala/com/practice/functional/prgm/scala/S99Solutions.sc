/**
  * P20: Remove the Kth element from a list.
  * Return the list and the removed element in a Tuple. Elements are numbered from 0.
  * Example:
  * *
  * scala> removeAt(1, List('a, 'b, 'c, 'd))
  * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  **/

def removeKthElem[T](k: Int, list: List[T]): (List[T], T) = {
  val result = list.foldLeft(List[T](), 0: Int, None: Option[T])((acc, c) => {
    if (acc._2 == k)
      (acc._1, acc._2 + 1, Some(c))
    else
      (c :: acc._1, acc._2 + 1, acc._3)
  })

  (result._1.reverse, result._3.get)

}

removeKthElem(4, List("a", "b", "c", "d"))


/**
  * P21 (*) Insert an element at a given position into a list.
  * Example:
  * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  */

