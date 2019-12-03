package com.scalaprogramming.courseera.functionaldesign.week1

/**
  * Objective of this Example is to show that "for" expression is not just associated with collection.
  * 'for' internally get translated to map and flatMap.
  * If the data structure has support for map and flatMap, it can leverage 'for' expression for simpler syntax.
  *
  * @tparam T
  */
sealed trait Generator[+T] {

  self =>

  def generate: T

  def map[U](f: T => U) = new Generator[U] {
    override def generate = f(self.generate)
  }

  def flatMap[U](f: T => Generator[U]) = new Generator[U] {
    override def generate: U = f(self.generate).generate
  }

}

/**
  * Base generator: Defines generators to reuse in next examples.
  */
object BaseGenerator {

  /**
    * Not an actual Generator as it always returns the value that is passed to it.
    * It's helpful to handle the boundary values.
    *
    * @param x
    * @tparam T
    * @return
    */
  def single[T](x: T): Generator[T] = new Generator[T] {
    override def generate: T = x
  }

  /**
    * Generate a java based Random Integer value.
    */
  val integers = new Generator[Int] {
    override def generate = {
      val rand = new java.util.Random()
      rand.nextInt()
    }

    // map and flatMap methods are inherited from the Generator Trait.
  }

  /**
    * We can use 'for' expression here on Generator as it supports map and flatMap method.
    * The below 'for' expression expands to the below expression
    * integers map(x => x > 0)
    */
  val booleans = for (x <- integers) yield x > 0

  /**
    * Explained towards the end of course era lecture. It showcases a method that use random data set generations
    * for asserting against a test condition.
    *
    * @param g
    * @param numTime
    * @param f
    * @tparam T
    */
  def test[T](g: Generator[T], numTime: Int = 100)(f: T => Boolean): Unit = {
    for {i <- 0 until numTime} {
      val value = g.generate
      assert(f(value), s"Failure for Value ${value}")
    }
    println(s"Passed ${numTime} tests")
  }
}

object Generate {

  import BaseGenerator._

  /**
    * pairs return a random pair of Generator of type T and U.
    * Again, we are able to express it in 'for' expression as the map and flatMap operations are defined on Generator Trait.
    *
    * @param t
    * @param u
    * @tparam T
    * @tparam U
    * @return
    */
  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    i <- t
    j <- u
  } yield (i, j)

  /**
    * Return a Generator which will select a random index value from the given array
    *
    * @param xs
    * @tparam T
    * @return
    */
  def oneOf[T](xs: T*): Generator[T] = for (idx <- choose(0, xs.length)) yield xs(idx)

  /**
    * Select a random value from a Given Range.
    *
    * @param lo
    * @param high
    * @return
    */
  def choose(lo: Int, high: Int): Generator[Int] = for (x <- integers) yield Math.abs(lo + x % (high - lo))

  /**
    * Returns a Random List Generator
    *
    * @return
    */
  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans // Flip a coin to decide the kind of list to generate.
    list <- if (isEmpty) emptyList else nonEmptyList
  } yield list

  def emptyList = single(Nil) // Single helps to return a generator of Type Nil.

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
