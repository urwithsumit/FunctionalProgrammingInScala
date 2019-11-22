package com.scalaprogramming.functional.prgm.scala

class NumOfElementInListTest extends FunSuite with BeforeAndAfterEach with BeforeAndAfterAll {

  test ("Size of List with Data") {
    assert(NumOfElementInList.length(List(1, 1, 2, 3, 5, 8)) == 6)
  }



  test("BST Construction") {
    val BST = BinarySearchTree[Int]()
    assert(BST.buildBST(List(3, 4, 2, 5, 6)).right == Some(4))
  }

}

