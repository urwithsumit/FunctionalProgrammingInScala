/***
  * Api functions available for SET
  */
val set = Set(1, 2, 3, 4, 4, 3) // apply is called implicitly
//set: scala.collection.immutable.Set[Int] = Set(1, 2, 3, 4)

val set2 = Set.apply(11, 12, 13, 14, 3) // use apply factory method
//set2: scala.collection.immutable.Set[Int] = HashSet(14, 13, 12, 3, 11)

set + 9
//res0: scala.collection.immutable.Set[Int] = HashSet(1, 9, 2, 3, 4)

set - 5
//res1: scala.collection.immutable.Set[Int] = Set(1, 2, 3, 4)

val set3 = Set[Int]().empty // Define an Empty Set of Int Type
//set3: scala.collection.immutable.Set[Int] = Set()

set3 + 4 // If set3 was not defined as Int type, this statment on an empty set would error out
//res2: scala.collection.immutable.Set[Int] = Set(4)

val concatSet = set.concat(set2).concat(set3)
//concatSet: scala.collection.immutable.Set[Int] = HashSet(14, 1, 13, 2, 12, 3, 11, 4)

val concatSet2 = set ++ set2 ++ set3 // ++ is alias for add all or Concat
//concatSet2: scala.collection.immutable.Set[Int] = HashSet(14, 1, 13, 2, 12, 3, 11, 4)

val intersect = set & set2 // intersection of 2 sets, here  Set(3)
//intersect: scala.collection.immutable.Set[Int] = Set(3)

val intersect2 = set.intersect(set2) //Set(3)
//intersect2: scala.collection.immutable.Set[Int] = Set(3)

val diff = set.diff(set2) //Set(1, 2, 4)
//diff: scala.collection.immutable.Set[Int] = Set(1, 2, 4)

//&~ Alias for diff
val diff2 = set &~ set2 //Set(1, 2, 4)
//diff2: scala.collection.immutable.Set[Int] = Set(1, 2, 4)

val remAll = set2 -- set //HashSet(14, 13, 12, 11)
//remAll: scala.collection.immutable.Set[Int] = HashSet(14, 13, 12, 11)

val adStr = remAll.addString(new StringBuilder, "[", ",", "]") //[14,13,12,11]
//adStr: StringBuilder = [14,13,12,11]

val adStr2 = remAll.addString(new StringBuilder, ",") //14,13,12,11
//adStr2: StringBuilder = 14,13,12,11

val adStr3 = remAll.addString(new StringBuilder) // no separator, values are all concatenated.
//adStr3: StringBuilder = 14131211

def doit(it: Set[Int]) = it.andThen(diff ++ diff2)
//doit: (it: Set[Int])Int => Boolean

diff.canEqual(diff2)
//res3: Boolean = true

val corr = diff2.collect(_ * 2)
//corr: scala.collection.immutable.Set[Int] = Set(2, 4, 8)

diff2.collectFirst(_ * 2)
//res5: Option[Int] = Some(2)

val arr = Array.ofDim[Int](diff2.size)
//arr: Array[Int] = Array(0, 0, 0)

val arr2 = Array.ofDim[Int](diff2.size, 4)
//arr2: Array[Array[Int]] = Array(Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0))

diff2.copyToArray(arr)
//res6: Int = 3

arr.mkString(",")
//res7: String = 1,2,4

// diff2 is Set(1,2,4) and corrs is diff2.collect(_ * 2)
// below d correspond to element from diff2 and c to corrs
// clearly there is a relation between d and c.
diff2.corresponds(corr)((d, c) => c == 2 * d)
//res7: Boolean = true

diff2.count(_ % 2 == 0)
//res8: Int = 2

diff2.diff(set2)
//res9: scala.collection.immutable.Set[Int] = Set(1, 2, 4)

diff2.drop(1)
//res10: scala.collection.immutable.Set[Int] = Set(2, 4)

diff2.dropRight(1)
//res11: scala.collection.immutable.Set[Int] = Set(1, 2)

diff2.dropWhile(_ % 2 == 0)
//res12: scala.collection.immutable.Set[Int] = Set(1, 2, 4)

diff2.empty
//res13: scala.collection.immutable.Set[Int] = Set()

diff2.equals(diff2.empty)
//res14: Boolean = false

diff2.equals(diff) // value check
//res15: Boolean = true

diff2.eq(diff) // Reference check
//res16: Boolean = false

diff2 == diff //value check
//res17: Boolean = true

diff2.exists(_ % 2 == 0)
//res18: Boolean = true

diff2.filter(_ % 2 == 0)
//res19: scala.collection.immutable.Set[Int] = Set(2, 4)

diff2.filterNot(_ % 2 == 0)
//res20: scala.collection.immutable.Set[Int] = Set(1)

diff2.find(_ % 2 == 0)
//res21: Option[Int] = Some(2)

val set4 = Set(diff2, diff, set, set2)
//set4: scala.collection.immutable.Set[scala.collection.immutable.Set[Int]] = Set(Set(1, 2, 4), Set(1, 2, 3, 4), HashSet(14, 13, 12, 3, 11))

set4.flatMap(c => c map (_ * 2))
//res22: scala.collection.immutable.Set[Int] = HashSet(24, 6, 28, 2, 22, 26, 8, 4)

set4.flatten
//res23: scala.collection.immutable.Set[Int] = HashSet(14, 1, 13, 2, 12, 3, 11, 4)

diff2.fold(0)((r, c) => r + c)
//res24: Int = 7

diff2.foldLeft(0)((r, c) => r + c)
//res25: Int = 7

diff2.foldRight(0)((c, r) => r + c)
//res26: Int = 7

diff2.forall(_ % 2 == 0)
//res27: Boolean = false

diff2.forall(_ > 0) // satisfies for all elements in collection.
//res28: Boolean = true

arr.foreach(_ * 2)
// for each returns unit.. hence no result is shown here

arr.mkString(",") // no change to the original collection by foreach above.
//res30: String = 1,2,4

diff2.groupBy(_ % 2 == 0)
//res31: scala.collection.immutable.Map[Boolean,scala.collection.immutable.Set[Int]] = HashMap(false -> Set(1), true -> Set(2, 4))

diff2.groupMap(identity)(_ * 2)
//res32: scala.collection.immutable.Map[Int,scala.collection.immutable.Set[Int]] = Map(1 -> Set(2), 2 -> Set(4), 4 -> Set(8))

diff2.groupBy(identity)
//Map[Int,scala.collection.immutable.Set[Int]] = HashMap(1 -> Set(1), 2 -> Set(2), 4 -> Set(4))

diff2.groupMap(identity)(_ => 1)
//res34: scala.collection.immutable.Map[Int,scala.collection.immutable.Set[Int]] = Map(1 -> Set(1), 2 -> Set(1), 4 -> Set(1))

List(1, 1, 2, 3, 4, 5).groupMapReduce(identity)(_ => 1)(_ + _)
//res35: scala.collection.immutable.Map[Int,Int] = HashMap(5 -> 1, 1 -> 2, 2 -> 1, 3 -> 1, 4 -> 1)

Set(1, 2, 3, 4, 5).groupMapReduce(key => key % 2)(identity)(_ + _)
//res36: scala.collection.immutable.Map[Int,Int] = Map(0 -> 6, 1 -> 9)

Set(1, 2, 3, 4, 5).groupMap(_ % 2)(identity)
//res37: scala.collection.immutable.Map[Int,scala.collection.immutable.Set[Int]] = Map(0 -> HashSet(2, 4), 1 -> HashSet(5, 1, 3))

Set(1, 2, 3, 4, 5).grouped(2).foreach(println(_))
//HashSet(5, 1)
//HashSet(2, 3)
//HashSet(4)

diff2.hashCode()
//res39: Int = 198479949

diff2.head
//res40: Int = 1

diff2.headOption
//res41: Option[Int] = Some(1)

diff2.init
//res42: scala.collection.immutable.Set[Int] = Set(1, 2)

Set(1, 2, 3, 4, 5).inits.foreach(println(_))
//HashSet(5, 1, 2, 3, 4)
//HashSet(5, 1, 2, 3)
//HashSet(5, 1, 2)
//HashSet(5, 1)
//HashSet(5)
//HashSet()

Set(1, 2, 3).intersect(Set(2, 3, 4, 5))
//res44: scala.collection.immutable.Set[Int] = Set(2, 3)

diff2.isEmpty
//res45: Boolean = false

diff2.knownSize
//res46: Int = 3

List(1, 2, 3, 5, 5, 5).knownSize
//res47: Int = -1

diff2.last
//res48: Int = 4

diff2.lastOption
//res49: Option[Int] = Some(4)

diff2.lazyZip(diff2).map {
  (r, c) => (r, c)
}
//res50: scala.collection.immutable.Set[(Int, Int)] = Set((1,1), (2,2), (4,4))

diff2.max
//res51: Int = 4

diff2.maxBy(_ >= 2) // returns 1st element encountered
//res52: Int = 2

diff2.maxOption
//res53: Option[Int] = Some(4)

diff2.maxByOption(_ >= 2)
//res54: Option[Int] = Some(2)

diff2.min
//res55: Int = 1

Set(1, 2, 3, 4).minBy(_ <= 2)
//res56: Int = 3

diff2.minOption
//res57: Option[Int] = Some(1)

Set(1, 2, 3, 4).minByOption(_ <= 2)
//res58: Option[Int] = Some(3)

Set(1, 2, 3, 4).partition(_ % 2 == 0)
//res59: (scala.collection.immutable.Set[Int], scala.collection.immutable.Set[Int]) = (Set(2, 4),Set(1, 3))

Set(1, 2, 3, 4).partitionMap {
  case i if i <= 2 => Left(i)
  case i if i > 2 => Right(i)
}
//res60: (scala.collection.immutable.Set[Int], scala.collection.immutable.Set[Int]) = (Set(1, 2),Set(3, 4))

Set(1, 2, 3, 4).product // product of all numbers in the set
//res61: Int = 24

Set(1, 2, 3, 4).reduce((r, c) => r + c)
//res62: Int = 10

Set(1, 2, 3, 4).reduce(_ + _)
//res63: Int = 10

Set(1, 2, 3, 4).reduceLeft(_ + _)
//res64: Int = 10

Set("a", "b", "c", "d").reduceLeft(_ + _)
//res65: String = abcd

Set(1, 2, 3, 4).reduceLeftOption(_ + _)
//res66: Option[Int] = Some(10)

Set("a", "b", "c", "d").reduceRight(_ + _)
//res67: String = abcd

Set("a", "b", "c", "d").reduceRightOption(_ + _)
//res68: Option[String] = Some(abcd)

Set(1, 2, 3, 4).removedAll(diff2)
//res69: scala.collection.immutable.Set[Int] = Set(3)

Set(1, 2, 3, 4).scan(0)(_ + _)
//res70: scala.collection.immutable.Set[Int] = HashSet(0, 10, 1, 6, 3)

Set(1, 2, 3, 4).scan(1)(_ * _)
//res71: scala.collection.immutable.Set[Int] = Set(1, 2, 6, 24)

Set(1, 2, 3, 4).scanLeft(0)(_ + _)
//res72: scala.collection.immutable.Set[Int] = HashSet(0, 10, 1, 6, 3)

Set(1, 2, 3, 4).scanRight(0)(_ + _)
//res73: scala.collection.immutable.Set[Int] = HashSet(0, 10, 9, 7, 4)

Set(1, 2, 3, 4).size
//res74: Int = 4

Set(1, 2, 3, 4).slice(1, 3)
//res75: scala.collection.immutable.Set[Int] = Set(2, 3)

Set(1, 2, 3, 4, 5).sliding(2, 2).foreach(println(_))
//HashSet(5, 1)
//HashSet(2, 3)
//HashSet(4)

Set(1, 2, 3, 4, 5).span(_ > 2)
//res77: (scala.collection.immutable.Set[Int], scala.collection.immutable.Set[Int]) = (HashSet(5),HashSet(1, 2, 3, 4))

diff2.view.map(_ + 3)
//res78: scala.collection.View[Int] = View(<not computed>)

diff2.zip(diff)
//res79: scala.collection.immutable.Set[(Int, Int)] = Set((1,1), (2,2), (4,4))

val ResMap = diff2.zipAll(Set(7, 8), -1, -2)
//res80: scala.collection.immutable.Set[(Int, Int)] = Set((1,7), (2,8), (4,-2))

diff2.zipWithIndex
//res81: scala.collection.immutable.Set[(Int, Int)] = Set((1,0), (2,1), (4,2))

diff | diff2
//res82: scala.collection.immutable.Set[Int] = Set(1, 2, 4)

diff2.zipWithIndex.unzip
//res82: (scala.collection.immutable.Set[Int], scala.collection.immutable.Set[Int]) = (Set(1, 2, 4),Set(0, 1, 2))

val xs = Set(
  Set(1, 2, 3),
  Set(4, 5, 6)).transpose
//xs: scala.collection.immutable.Set[scala.collection.immutable.Set[Int]] = Set(Set(1, 4), Set(2, 5), Set(3, 6))

Set(2, 3, 4, 5, 6).subsets(2).foreach(println(_))
//HashSet(5, 6)
//HashSet(5, 2)
//HashSet(5, 3)
//HashSet(5, 4)
//HashSet(6, 2)
//HashSet(6, 3)
//HashSet(6, 4)
//HashSet(2, 3)
//HashSet(2, 4)
//HashSet(3, 4)