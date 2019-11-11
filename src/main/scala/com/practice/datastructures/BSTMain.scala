package com.practice.datastructures

object BSTMain extends App {

  val BST = new BinarySearchTree[Int]()

  BST.buildBST(List(6, 3, 4, 5, 9, 1))

  println(s"${BST.inOrder()}")

  println(s"${BST.preOrder()}")

  println(s"${BST.postOrder()}")

  println(s"${BST.search(6).getOrElse().toString}")

  println(s"${BST.search(3).getOrElse().toString}")

  println(s"${BST.search(4).getOrElse().toString}")

  println(s"${BST.search(5).getOrElse().toString}")

  println(s"${BST.search(9).getOrElse().toString}")

  println(s"${BST.search(1).getOrElse().toString}")

  println(s"${BST.treeMax().toString}")

  println(s"${BST.treeMin().toString}")

  println(s"${BST.treeSuccessor().toString}")


}
