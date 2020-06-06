trait User {
  def username: String
}

trait Tweeter {
  // self declaration here forces for any class implementing the Tweeter class to mix-in User trait as well.
  self: User =>
  def tweets(tText: String) = println(s"${username}: $tText")
}

class TweeterUser(val uName: String) extends Tweeter with User {
  override def username: String = uName
}

val _sumit = new TweeterUser("Sumit")
_sumit.tweets("What a Day to do coding!!")

import scala.language.implicitConversions

implicit def list2ordered[A](x: List[A])(implicit that: A => Ordered[A]): Ordered[List[A]] =
  new Ordered[List[A]] {
    def compare(that: List[A]): Int = 1
  }

def whileLoop(test: => Boolean)(body: => Unit): Unit = {
  if (test) {
    body
    whileLoop(test)(body)
  }
}

var i = 10
whileLoop(i > 0) {
  println(i)
  i -= 1
}


def allPathsSourceTarget(graph: Array[Array[Int]]): List[List[Int]] = {
  def paths(source: Int, target: Int): List[List[Int]] = {
    if (source == target) List(List(target))
    else (for (n <- graph(source); p <- paths(n, target)) yield source :: p).toList
  }

  paths(0, graph.length - 1)
}
allPathsSourceTarget(Array(Array(1, 2), Array(3), Array(3), Array()))





