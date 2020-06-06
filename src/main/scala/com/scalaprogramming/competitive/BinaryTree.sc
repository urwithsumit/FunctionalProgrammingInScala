import com.sun.tools.hat.internal.model.Root

/**
* Iterative Preorder: Parent -> Left -> Right
*/
def preorderTraversal(root: TreeNode): List[Int] = {

  val Result = scala.collection.mutable.ListBuffer[Int]()
  val stack = scala.collection.mutable.Stack[TreeNode]()

  if (root != null) {

    var current = root

    while (current != null || !stack.isEmpty) {
      while (current != null) {
        Result.addOne(current.value)
        stack.push(current)
        current = current.left
      }

      current = stack.pop()
      current = current.right
    }
  }

  Result.toList
}

/***
* Iterative Inorder Traversal Left -> Parent -> Right
*/
def inorderTraversal(root: TreeNode): List[Int] = {

  val stack = scala.collection.mutable.Stack[TreeNode]()
  val Result = scala.collection.mutable.ListBuffer[Int]()

  var current = root

  while (current != null || !stack.isEmpty) {

    while (current != null) {
      stack.push(current)
      current = current.left
    }

    current = stack.pop()

    if (current != null) {
      Result.addOne(current.value)
      current = current.right
    }
  }

  Result.toList
}

/***
*  Iterative Post order traversal.
*
*  //TODO not Working....
*/
def postorderTraversal(root: TreeNode): List[Int] = {

  val stack = scala.collection.mutable.Stack[TreeNode]()
  val Result = scala.collection.mutable.ListBuffer[Int]()

  var current = root

  while (current != null || !stack.isEmpty) {

    while (current != null) {
      stack.push(current)
      current = current.left
    }

    current = stack.pop()

    if (current != null) {
      Result.addOne(current.value)
      current = current.right
    }

  }

  Result.toList
}