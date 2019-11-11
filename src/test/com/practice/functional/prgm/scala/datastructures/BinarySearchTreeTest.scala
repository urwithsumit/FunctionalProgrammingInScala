package com.practice.functional.prgm.scala.datastructures

class BinarySearchTreeTest extends FunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  val BST = BinarySearchTree[Int]()

  test("BST Construction") {
    assert(BST.buildBST(List(3, 4, 2, 5, 6)).right == Some(4))
  }
}