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

removeKthElem(2, List("a", "b", "c", "d"))

////////////////////////////////////////////////////////////////////////

/**
 * P21 (*) Insert an element at a given position into a list.
 * Example:
 * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
 * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
 */

def insertAt[T](k: T, n: Int, list: List[T]): List[T] = {
  val result = list.foldLeft(List[T](), 0: Int)((r, c) =>
    if (r._2 == n)
      (c :: k :: r._1, r._2 + 1)
    else
      (c :: r._1, r._2 + 1))

  result._1.reverse
}

insertAt("S", 2, List("a", "b", "c", "d"))

////////////////////////////////////////////////////////////////////////

/**
 * P22 (*) Create a list containing all integers within a given range.
 * Example:
 * scala> range(4, 9)
 * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
 */

def range[T](start: Int, end: Int) = {
  for (i <- start to end) yield i
  }.toList

range(3, 9)

range(17, 9)

////////////////////////////////////////////////////////////////////////

/**
 * P23 (**) Extract a given number of randomly selected elements from a list.
 * Example:
 * scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
 * res0: List[Symbol] = List('e, 'd, 'a)
 *
 */

def randomSelect[T](k: Int, list: List[T]): List[T] = {
  val Rand = new scala.util.Random()

  val index = for (i <- 1 to k) yield {
    Rand.nextInt(list.length - 1)
  }

  index.foldLeft(List[T]())((r, c) => list(c) :: r)
}

randomSelect(3, List("a", "b", "c", "d", "e", "f", "g", "h", "I", "j"))

/**
 * P24 (*) Lotto: Draw N different random numbers from the set 1..M.
 * Example:
 * scala> lotto(6, 49)
 * res0: List[Int] = List(23, 1, 17, 33, 21, 37)
 */

def lotto[T](n: Int, m: Int) = randomSelect(n, range(1, m))
lotto(6, 49)

/**
 * P25 (*) Generate a random permutation of the elements of a list.
 * Hint: Use the solution of problem P23.
 * Example:
 *
 * scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
 * res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
 */

def shuffle[T](ls: List[T]): List[T] = randomSelect(ls.length, ls)

shuffle(List("a", "b", "c", "d", "e", "f", "g", "h", "I", "j"))
shuffle(List("a", "b", "c", "d", "e", "f", "g", "h", "I", "j"))
shuffle(List("a", "b", "c", "d", "e", "f", "g", "h", "I", "j"))
shuffle(List("a", "b", "c", "d", "e", "f", "g", "h", "I", "j"))

