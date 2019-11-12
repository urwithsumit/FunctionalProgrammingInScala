package com.practice.datastructures


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
    * @param n
    * @param current
    * @return
    */
  def delete(n: T, current: Option[Node] = root): Boolean = ???

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
        print(node.value + " ")
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
    * @param current
    * @return
    */
  def treeSuccessor(current: Option[Node] = root): Option[Node] = {
    current match {
      case Some(node) if node.hasRightNode => treeMin(node.right)
      case Some(node) if !node.hasRightNode => Some(node)
      // TODO : Algorithm, as explained in Corman, is not implemented fully.
      // TODO Scenario, when the Right child does not exist, is pending
    }
  }

  /**
   * Find the True Predecessor for a node. Symmetric to tree Successor algorithm.
   * Maximum element in the left sub-tree.
   * @param current
   * @return
   */
  def treePredecessor(current: Option[Node] = root): Option[Node] = {
    current match {
      case Some(node) if node.hasLeftNode => treeMax(node.left)
      case Some(node) if !node.hasLeftNode => Some(node)
      // TODO : Algorithm, as explained in Corman, is not implemented fully.
      // TODO Scenario, when the Right child does not exist, is pending
    }
  }


  case class Node(var left: Option[Node], var right: Option[Node], var parent: Option[Node], value: T) {
    override def toString: String =
      s"""
         |Node[
         |  Value = ${value},
         |  Parent = ${if (isParent) parent.get.value else None},
         |  Left = ${if (hasLeftNode) left.get.value else None},
         |  Right = ${if (hasRightNode) right.get.value else None}
         |]
       """.stripMargin

    def isParent = parent.isDefined

    def isLeaf = !hasLeftNode && !hasRightNode

    def hasLeftNode = left.isDefined

    def hasRightNode = right.isDefined
  }

}
