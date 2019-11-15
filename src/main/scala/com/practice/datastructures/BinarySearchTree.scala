package com.practice.datastructures

sealed abstract class Tree[+T]

/**
  * Binary Tree operations.
  *
  * @param ord
  * @tparam T
  */
class BinarySearchTree[T](implicit ord: Ordering[T]) {

  private final var root: Option[Node] = None

  /**
    *
    * @param list
    * @tparam
    * @return
    */
  def buildBST(list: List[T]) = {
    list match {
      case Nil => throw new IllegalArgumentException("Missing Values for the Tree.")
      case head :: tail =>
        root = Some(Node(None, None, None, head))
        tail.foreach(insert(_))
    }
  }

  def printTree = {
    println(root.getOrElse(None).toString)
  }

  /**
    * Add an element to a Binary Tree
    *
    * @param n
    * @tparam
    */
  def insert(n: T, current: Option[Node] = root): Unit = {

    current match {
      case Some(node) if (ord.lt(n, node.value)) => {
        if (node.hasLeftNode)
          insert(n, node.left)
        else {
          node.left = Some(Node(None, None, current, n))
        }
      }

      case Some(node) if (ord.gteq(n, node.value)) => {
        if (node.hasRightNode)
          insert(n, node.right)
        else {
          node.right = Some(Node(None, None, current, n))
        }
      }

      case _ => Some(Node(None, None, None, n))
    }
  }

  /**
    * Delete a Node.
    *
    * @param n
    * @return
    */
  def delete(n: T): Boolean = {


    search(n) match {
      // Case Root is the Only Node in the Tree
      case Some(node) if (ord.compare(node.value, n) == 0 && node.isLeaf && node.getParent == this) => {
        root = None
        true
      }
      // Case Node is a leaf node.
      case Some(node) if (ord.compare(node.value, n) == 0 && node.isLeaf) => {
        if (node.isLeftChild)
          node.getParent.left = None
        else
          node.getParent.right = None

        true
      }
      case Some(node) if (ord.compare(node.value, n) == 0 && node.isLeftChild && node.hasLeftNode && node.hasRightNode) => {


        true
      }
      case Some(node) if (ord.compare(node.value, n) == 0 && node.isLeftChild && node.hasLeftNode) => {
        node.getParent


        true
      }
      case Some(node) if (ord.compare(node.value, n) == 0 && node.isLeftChild && node.hasRightNode) => {


        true
      }
      case Some(node) if (ord.compare(node.value, n) == 0 && node.isRightChild && node.hasLeftNode && node.hasRightNode) => {


        true
      }
      case Some(node) if (ord.compare(node.value, n) == 0 && node.isRightChild && node.hasLeftNode) => {
        node.getParent


        true
      }
      case Some(node) if (ord.compare(node.value, n) == 0 && node.isRightChild && node.hasRightNode) => {


        true
      }
      case _ => false
    }

  }

  /**
    * Search an Element in a given Binary Search Tree
    *
    * @param key
    * @param current
    * @tparam
    * @return
    */
  def search(key: T, current: Option[Node] = root): Option[Node] = {
    current match {
      case Some(node) if node.value == key => current
      case Some(node) if ord.lt(key, node.value) => search(key, node.left)
      case Some(node) if ord.gteq(key, node.value) => search(key, node.right)
      case _ => None
    }
  }

  /**
    * Travel in-order i.e. Left Node, Parent, Right Node.
    *
    * @param current
    * @tparam
    * @return
    */
  def inOrder(current: Option[Node] = root): Unit = {

    current match {
      case Some(node) => {
        inOrder(node.left)
        print(node.toString + " ")
        inOrder(node.right)
      }
      case _ =>
    }

  }

  /**
    * Travel Pre-order i.e. Parent, Left Node, Right Node.
    *
    * @param current
    * @tparam
    * @return
    */
  def preOrder(current: Option[Node] = root): Unit = {
    current match {
      case Some(node) => {
        print(node.value + " ")
        preOrder(node.left)
        preOrder(node.right)
      }
      case _ =>
    }
  }

  /**
    * Travel Post-order i.e. Left Node, Right Node, Parent.
    *
    * @param current
    * @tparam
    * @return
    */
  def postOrder(current: Option[Node] = root): Unit = {
    current match {
      case Some(node) => {
        postOrder(node.left)
        postOrder(node.right)
        print(node.value + " ")
      }
      case _ =>
    }
  }

  /**
    * Get the maximum element of a tree
    *
    * @param current
    * @return
    */
  def treeMax(current: Option[Node] = root): Option[Node] = {
    current match {
      case Some(node) if (node.hasRightNode) => treeMax(node.right)
      case _ => current
    }
  }

  /**
    * Get the minimum element of a tree
    *
    * @param current
    * @return
    */
  def treeMin(current: Option[Node] = root): Option[Node] = {
    current match {
      case Some(node) if (node.hasLeftNode) => treeMin(node.left)
      case _ => current
    }
  }

  /**
    * Height of a Tree.
    *
    * @param current
    * @return
    */
  def height(current: Option[Node] = root): Int = ???

  /**
    * Find the True successor for a Node.
    * Minimum element in the Right sub-tree.
    *
    * @param current
    * @return
    */
  def treeSuccessor(current: Option[Node] = root): Option[Node] = {
    current match {
      case Some(node) if node.hasRightNode => treeMin(node.right)
      case _ => current
      // TODO : Algorithm, as explained in Cormen, is not implemented fully.
      // TODO Scenario, when the Right child does not exist, is pending
    }
  }

  /**
    * Find the True Predecessor for a node. Symmetric to tree Successor algorithm.
    * Maximum element in the left sub-tree.
    *
    * @param current
    * @return
    */
  def treePredecessor(current: Option[Node] = root): Option[Node] = {
    current match {
      case Some(node) if node.hasLeftNode => treeMax(node.left)
      case _ => current
      // TODO : Algorithm, as explained in Cormen, is not implemented fully.
      // TODO Scenario, when the Right child does not exist, is pending
    }
  }

  /**
    * Count the Number of Leaf in a Tree.
    *
    * @return
    */
  def countLeaf: Option[Int] = {
    val Q = scala.collection.mutable.Queue(root)
    var r = 0
    while (!Q.isEmpty) {
      Q.dequeue() match {
        case Some(x) if (x.isLeaf) => r += 1
        case Some(x) if (x.hasLeftNode && x.hasRightNode) => Q.enqueue(x.left, x.right);
        case Some(x) if (x.hasLeftNode) => Q.enqueue(x.left);
        case Some(x) if (x.hasRightNode) => Q.enqueue(x.right);
        case _ =>
      }
    }

    Some(r)
  }


  case class Node(var left: Option[Node], var right: Option[Node], var parent: Option[Node], value: T) extends Tree[T] {
    override def toString: String =
      s"""
         |Node [Value = ${value}, Left = ${if (hasLeftNode) left.get.value.toString else None}, Right = ${if (hasRightNode) right.get.value.toString else None}, Parent = ${if (hasParentNode) parent.get.value.toString else None}]
       """.stripMargin

    def hasParentNode = parent.isDefined

    def isRightChild = !isLeftChild

    def isLeftChild = parent match {
      case Some(node) if (ord.lt(value, node.value)) => true
      case _ => false
    }

    def isLeaf = !hasLeftNode && !hasRightNode

    def hasLeftNode = left.isDefined

    def hasRightNode = right.isDefined

    def getParent: Node = parent match {
      case Some(node) => node
      case _ => this // Root is the parent of itself
    }

  }

}

object BinarySearchTree extends App {

  implicit def printNode[T](v: Option[T]) = v match {
    case Some(x) => print(x.toString)
    case _ => print("None")
  }

  val BST = new BinarySearchTree[Int]()

  BST.buildBST(List(6, 3, 4, 5, 9, 1))

  println("Print Tree Root: ")
  BST.printTree
  println("\n")

  println(s"In Order Traversal: ")
  BST.inOrder()
  println("\n")

  println(s"Pre Order Traversal: ")
  BST.preOrder()
  println("\n")

  println(s"Post Order Traversal")
  BST.postOrder()
  println("\n")

  println(s"Search Key: 6")
  printNode(BST.search(6))
  println("\n")

  println(s"Tree Max Node: ")
  printNode(BST.treeMax())
  println("\n")

  println(s"Tree Min Node: ")
  printNode(BST.treeMin())
  println("\n")

  println(s"Tree Successor Node: ")
  printNode(BST.treeSuccessor())
  println("\n")

  println(s"Tree Predecessor Node: ")
  printNode(BST.treePredecessor())
  println("\n")

  println(s"Leaf Count in Tree: ")
  printNode(BST.countLeaf)

}

