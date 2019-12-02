package com.scalaprogramming.courseera.functionaldesign.week1

/**
  *
  * @tparam T
  */
sealed trait Generator[+T] {

  s =>

  def generate: T

  def map[U](f: T => U) = new Generator[U] {
    override def generate = f(s.generate)
  }

  def flatMap[U](f: T => Generator[U]) = new Generator[U] {
    override def generate: U = f(s.generate).generate
  }

}

object BaseGenerator {
  val integers = new Generator[Int] {
    override def generate = {
      val rand = new java.util.Random()
      rand.nextInt()
    }
  }

  val booleans = for (x <- integers) yield x > 0

  def test[T](g: Generator[T], numTime: Int = 100)(test: T => Boolean): Unit = {
    for {i <- 0 until numTime} {
      val value = g.generate
      assert(test(value), s"Failure for Value ${value}")
    }
    println(s"Passed ${numTime} tests")
  }
}

object Generate {

  import BaseGenerator._

  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    i <- t
    j <- u
  } yield (i, j)

  def single[T](x: T): Generator[T] = new Generator[T] {
    override def generate: T = x
  }

  def oneOf[T](xs: T*): Generator[T] = for (idx <- choose(0, xs.length)) yield xs(idx)

  def choose(lo: Int, high: Int): Generator[Int] = for (x <- integers) yield Math.abs(lo + x % (high - lo))

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyList else nonEmptyList
  } yield list

  def emptyList = single(Nil)

  def nonEmptyList = for {
    head <- integers
    tail <- lists
  } yield head :: tail

}

trait Tree {

  import BaseGenerator._

  val leafs: Generator[Leaf] = for {
    i <- integers
  } yield Leaf(i)

  def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree

  case class Inner(left: Tree, right: Tree) extends Tree

  case class Leaf(x: Int) extends Tree

}

object Tree extends Tree

object Test extends App {

  import BaseGenerator._
  import Generate._

  println(pairs(integers, booleans).generate)

  println(oneOf("S", "Sh", "D", "K", "A", "B").generate)

  println(lists.generate)

  println(Tree.trees.generate)

  test(pairs(lists, lists), 100) {
    case (xs, ys) => (xs ++ ys).length > xs.length
  }
}
