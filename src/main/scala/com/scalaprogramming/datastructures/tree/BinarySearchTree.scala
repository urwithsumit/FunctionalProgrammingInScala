package com.scalaprogramming.datastructures.tree

import com.scalaprogramming.datastructures.tree.BinarySearchTree.BST

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
    * @Cormen
    * @param n
    * @return
    */
  def delete(n: T) = {
    println(s"Delete: ${n}")

    // Splice a node by linking the parent and successor node.
    def transplant(node: Node, successor: Option[Node]) = {
      if (!node.hasParentNode) root = successor
      else if (node.isLeftChild) node.getParent.left = successor
      else node.getParent.right = successor

      if (successor.isDefined) successor.get.parent = node.parent
    }

    search(n) match {
      // Case Root is the Only Node in the Tree
      case Some(node) if (ord.compare(node.value, n) == 0 && node.isLeaf && node.getParent == this) => {
        root = None
      }
      // Case Node is a leaf node: In this case there is no child dependency to manage, hence simply check if the value is less than parent,
      // than assign the Left Child of Parent to None else assign the right child of parent to None.
      case Some(node) if (ord.compare(node.value, n) == 0 && node.isLeaf) => {
        if (node.isLeftChild)
          node.getParent.left = None
        else
          node.getParent.right = None
      }
      // If node has Only Right child, then Right child is the successor.
      case Some(node) if (ord.compare(node.value, n) == 0) && node.hasRightNode && !node.hasLeftNode => {
        transplant(node, node.right)
      }
      // If node has Only Left child, then Left child is the successor.
      case Some(node) if (ord.compare(node.value, n) == 0) && !node.hasRightNode && node.hasLeftNode => {
        transplant(node, node.left)
      }
      // Node has a Left Node and a Right Node.
      case Some(node) if (ord.compare(node.value, n) == 0 && node.hasLeftNode && node.hasRightNode) => {

        // Take the minimum node of the Right subtree.
        treeMin(node.right) match {
          // If Successor is the immediate Right Child of node, than transplant.
          case Some(successor) if successor.parent.get eq node =>
            // Since Successor is immediate Right node, So transplant the Successor as is with Node.
            transplant(node, Some(successor))

            // Successor left node to be updated with node's left child. Note: We do not need to update the Right child of Successor,
            // as we have transplanted the complete right node with node.
            successor.left = node.left

          // If Successor is not the Immediate Right Child of the node, the transplant happens
          // in 2 steps:
          case Some(successor) =>
            // Transplant Successor's Right Child to Successor. Note: Minimum Node will have no left Child.
            transplant(successor, successor.right)

            // Successor's Right child can now be update with Node's right child.
            successor.right = node.right

            // Now transplant node with the Successor.
            transplant(node, Some(successor))

            //Successor's Left Child Can now be updated with Node's left child.
            successor.left = node.left
        }


      }

      case None => throw new IllegalArgumentException(s"Delete Failure: Tree does not contain ${n}")
    }

    println(s"Resulting Tree: ")
    BST.pretty()
    println("\n")

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
    *
    * @param current
    */
  def BalancedTree(current: Option[Node] = root): Unit = {

    var buffer = mutable.ListBuffer[T]()

    //def SplitPoint(buffy: ListBuffer[T]) = if (buffy.length % 2 == 0) buffy.length / 2 else buffy.length / 2 + 1

    def dfs(node: Option[Node]): Unit = {
      node match {
        case Some(Node(left, right, parent, value)) => {
          dfs(left)
          buffer = (buffer.addOne(value))
          dfs(right)
        }
        case _ =>
      }
    }

    dfs(current)
    println(buffer.mkString(", "))
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
  def height(current: Option[Node] = root): Int = {
    if (current.getOrElse(null) == null) 0
    else math.max(height(current.get.left), height(current.get.right)) + 1
  }

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

  /**
    * Diameter of a binary tree
    *
    * @param node
    * @return
    */
  def diameterOfBinaryTree(node: Option[Node] = root): Int = {

    def _diameter(node: Option[Node]): Int = {
      if (node.getOrElse(null) == null || node.get.left == null && node.get.right == null) 0
      else math.max(height(node.get.left) + height(node.get.right), math.max(_diameter(node.get.left), _diameter(node.get.right)))
    }

    _diameter(node)
  }


/*
  def isValidBST(root: Option[Node] = root): Boolean = {

    def _isBST[K <: T](root: Option[Node], lo: K = Int.MinValue, hi: K = Int.MaxValue): Boolean = {
      root match {
        case Some(node) => {
          ord.compare(node.value, lo) > 0 && ord.compare(node.value, hi) < 0 && _isBST(node.left, lo, node.value) && _isBST(node.right, node.value, hi)
        }
        case None => true
      }
    }

    _isBST(root)

  }
*/

  /**
    * I am lazy to write this method, Hence taking it from below post
    * https://stackoverflow.com/questions/4965335/how-to-print-binary-tree-diagram
    *
    * @param node
    * @return
    */
  def pretty(node: Option[Node] = root) = {

    def work(tree: Node, prefix: String, isTail: Boolean): String = {
      val (line, bar) = if (isTail) ("└── ", " ") else ("├── ", "│")

      val curr = s"${prefix}${line}${tree.value}"

      val rights = tree.right match {
        case None => s"${prefix}${bar}   ├── ∅"
        case Some(r) => work(r, s"${prefix}${bar}   ", false)
      }

      val lefts = tree.left match {
        case None => s"${prefix}${bar}   └── ∅"
        case Some(l) => work(l, s"${prefix}${bar}   ", true)
      }

      s"${curr}\n${rights}\n${lefts}"

    }

    println(work(node.get, "", true))
  }

  case class Node(var left: Option[Node], var right: Option[Node], var parent: Option[Node], value: T) extends Tree[T] {
    override def toString: String =
      s"""
         |Node [Value = ${value}, Left = ${if (hasLeftNode) left.get.value.toString else None}, Right = ${if (hasRightNode) right.get.value.toString else None}, Parent = ${if (hasParentNode) parent.get.value.toString else None}]
       """.stripMargin

    def hasParentNode = parent.isDefined

    def isLeaf = !hasLeftNode && !hasRightNode

    def hasLeftNode = left.isDefined

    def hasRightNode = right.isDefined

    def hasSibling: Boolean = if (isRightChild) getParent.hasLeftNode else getParent.hasRightNode

    def isRightChild = !isLeftChild

    def isLeftChild = parent match {
      case Some(node) if (ord.lt(value, node.value)) => true
      case _ => false
    }

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

  BST.buildBST(List(6, 3, 4, 5, 1, 7, 11, 9, 8, 10, 13))

  println("Diameter =>" + BST.diameterOfBinaryTree())

  println("Print Tree: ")
  BST.pretty()
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
  BST.pretty(BST.search(6))
  println("\n")

  println(s"Tree Max Node: ")
  BST.pretty(BST.treeMax())
  println("\n")

  println(s"Tree Min Node: ")
  BST.pretty(BST.treeMin())
  println("\n")

  println(s"Tree Successor Node: ")
  BST.pretty(BST.treeSuccessor())
  println("\n")

  println(s"Tree Predecessor Node: ")
  BST.pretty(BST.treePredecessor())
  println("\n")

  println(s"Leaf Count in Tree: ")
  printNode(BST.countLeaf)
  println("\n")

  //BST.inOrder()
  BST.BalancedTree()
  //BST.inOrder()

  //println("Is a valid BST:: " + BST.isValidBST())

  println(BST.pretty())
  /* BST.delete(3)
   BST.delete(10)
   BST.delete(7)
   BST.delete(6)
   BST.delete(13)
 */


}

