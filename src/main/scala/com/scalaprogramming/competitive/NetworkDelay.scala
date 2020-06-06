package com.scalaprogramming.competitive

object NetworkDelay {

  def main(args: Array[String]): Unit = {

    println(networkDelayTime(Array(Array(2, 1, 1), Array(2, 3, 1), Array(3, 4, 1)), 4, 2))
    println(networkDelayTime(Array(Array(1, 2, 1)), 2, 1))
    println(networkDelayTime(Array(Array(1, 2, 1), Array(2, 1, 3)), 2, 2))

  }

  def networkDelayTime(times: Array[Array[Int]], N: Int, K: Int): Int = {

    val adjMap = scala.collection.mutable.Map.empty[Int, Set[(Int, Int)]]

    def buildGraph(u: Int, v: Int, w: Int, biDirectional: Boolean = true) = {
      adjMap.update(u, adjMap.getOrElse(u, Set()) + ((v, w)))
      if (biDirectional)
        adjMap.update(v, adjMap.getOrElse(v, Set()) + ((u, w)))
    }

    times.map(c => {
      buildGraph(c(0), c(1), c(2))
    })

    val pQueue = scala.collection.mutable.PriorityQueue.empty[(Int, Int)]
    val distance = Array.fill[Int](adjMap.size)(Integer.MAX_VALUE)
    val previous = Array.fill[Int](adjMap.size)(-1)

   /* def dijkastra(graph: Map[Int,(Int, Int)], source: Int) = {
      distance.update(source, 0)

    }
*/

    -1

  }

}
