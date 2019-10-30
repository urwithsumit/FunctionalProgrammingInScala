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
lotto(6,49)




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




/**
  * P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
  * In how many ways can a committee of 3 be chosen from a group of 12 people?
  * We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient).
  * For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
  * Example:
  *
  * scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
  * res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
  *
  **/

//Pending Solution.


/**
  * P27 (**) Group the elements of a set into disjoint subsets.
  * a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
  * Example:
  * *
  * scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
  * res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
  * b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
  * *
  * Example:
  * *
  * scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
  * res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
  * Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
  */

// Pending Solution.


/**
  * P28 (**) Sorting a list of lists according to length of sublists.
  * a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length.
  * E.g. short lists first, longer lists later, or vice versa.
  * Example:
  * *
  * scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
  * res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
  * b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their length frequency;
  * i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
  * *
  * Example:
  * *
  * scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
  * res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
  * Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once.
  * The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.
  *
  */

def lsort[T](ls: List[List[T]]) = ls.map(x => (x, x.size)).sortBy(_._2).map(_._1)
def lsort2[T](ls: List[List[T]]) = ls sortWith { _.length < _.length }

lsort(List(List("a", "b", "c"), List("d", "e"), List("f", "g", "h"), List("d", "e"), List("i", "j", "k", "l"), List("m", "n"), List("o")))
lsort2(List(List("a", "b", "c"), List("d", "e"), List("f", "g", "h"), List("d", "e"), List("i", "j", "k", "l"), List("m", "n"), List("o")))


def lsortFreq[T](ls: List[List[T]]) = ls.map(x => (x, x.size)).groupBy(_._2).values.map(c => (c, c.size)).toList.sortBy(_._2).flatMap(y => y._1.map(_._1))
lsortFreq(List(List("a", "b", "c"), List("d", "e"), List("f", "g", "h"), List("d", "e"), List("i", "j", "k", "l"), List("m", "n"), List("o")))
