class LRUCache(_capacity: Int) {

  private val cache = scala.collection.mutable.LinkedHashMap[Int, Int]().empty

  def get(key: Int): Int = {
    if (cache.contains(key)) {
      // Make a re-entry, before returning the value.
      val value = cache.getOrElse(key, -1)
      cache.remove(key)
      put(key, value)
    }

    cache.getOrElse(key, -1)
  }

  def put(key: Int, value: Int) {
    // Update Cache
    if (cache.contains(key)) {
      // make a re-entry.
      cache.remove(key)
      cache.put(key, value)
    } else {
      // Create new Entry and Re-adjust size.
      if (cache.size == _capacity) {
        cache.remove(cache.head._1)
      }
      cache.put(key, value)
    }

  }
}

///////////////////////////////////////
/*
val cache = new LRUCache(2)

cache.put(1, 1)
cache.put(2, 2)
cache.get(1) // returns 1
cache.put(3, 3) // evicts key 2
cache.get(2) // returns -1 (not found)
cache.put(4, 4) // evicts key 1
cache.get(1)
cache.get(3) // returns 3
cache.get(4)
*/

///////////////////////////////////////
val cache = new LRUCache(2)

cache.get(2)
cache.put(2, 6)
cache.get(1) // returns 1
cache.put(1, 5) // evicts key 2
cache.put(1, 2) // evicts key 1
cache.get(1)
cache.get(2) // returns 3

//////////////////////////
/**
* Implement Queue using Stack
*/
class MyQueue() {

  /** Initialize your data structure here. */
  private val stack = scala.collection.mutable.Stack[Int]()
  private val auxStack = scala.collection.mutable.Stack[Int]()

  private def headAvl = {
    if (!stack.isEmpty) move

    !empty
  }

  private def move = {
    while (!stack.isEmpty) {
      auxStack.push(stack.pop)
    }
  }

  private def moveback = {
    while (!auxStack.isEmpty) {
      stack.push(auxStack.pop)
    }
  }

  /** Push element x to the back of queue. */
  def push(x: Int) {
    if (!empty) moveback

    stack.push(x)
  }

  /** Removes the element from in front of queue and returns that element. */
  def pop(): Int = if (headAvl) auxStack.pop else -1

  /** Get the front element. */
  def peek(): Int = if (headAvl) auxStack.top else -1

  /** Returns whether the queue is empty. */
  def empty(): Boolean = stack.isEmpty && auxStack.isEmpty

}
