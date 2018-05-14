object CheckBST {

  def main(args: Array[String]): Unit = {
    val root = TreeNode(1)
    root.left = TreeNode(2)
    root.right = TreeNode(3)

    val answer = isValidBST(root)
    assert(!answer)

    val root2 = TreeNode(2)
    root2.left = TreeNode(1)
    root2.right = TreeNode(3)

    val answer2 = isValidBST(root2)
    assert(answer2)

    val root3 = TreeNode(75)
    val root3Left = TreeNode(35)
    val root3Right = TreeNode(85)
    root3.left = root3Left
    root3.right = root3Right
    val leftLeft = TreeNode(25)
    val leftRight = TreeNode(45)
    val rightLeft = TreeNode(80)
    val rightRight = TreeNode(95)

    root3Left.left = leftLeft
    root3Left.right = leftRight

    root3Right.left = rightLeft
    root3Right.right = rightRight


    val answer3 = isValidBST(root3)
    assert(answer3)

    // complex example
    val root4 = TreeNode(75)
    val root4Left = TreeNode(35)
    val root4Right = TreeNode(85)
    root4.left = root4Left
    root4.right = root4Right
    val leftLeft4 = TreeNode(25)
    val leftRight4 = TreeNode(45)
    val rightLeft4 = TreeNode(80)
    val rightRight4 = TreeNode(95)

    root4Left.left = leftLeft4
    root4Left.right = leftRight4

    root4Right.left = rightLeft4
    root4Right.right = rightRight4

    leftLeft4.right = TreeNode(30)
    leftRight4.left = TreeNode(40)

    rightLeft4.left = TreeNode(77)
    rightRight4.left = TreeNode(90)
    rightRight4.right = TreeNode(100)


    val answer4 = isValidBST(root4)
    assert(answer4)

    val root5: TreeNode = null
    assert(isValidBST(root5))

    assert(isValidBST(TreeNode(0)))
  }

  def isValidBST(root: TreeNode): Boolean = {

    def _check(present: TreeNode,
               parent: TreeNode,
               isLeft: Boolean): Boolean = {

      if (present == null) {
        // Base case
        true
      } else {
        //  println(present.value)
        if (isLeft) {
          //  println("left")
          present.value < parent.value &&
            // left subtree right child must be greater than the present, but smaller than the parent
            (present.right == null || present.right.value < parent.value) &&
            _check(present.left, present, isLeft = true) && _check(present.right, present, isLeft = false)
        } else {
          //  println("right")
          present.value > parent.value &&
            // right subtree left child must be smaller than the present, but greater than the parent
            (present.left == null || present.left.value > parent.value) &&
            _check(present.left, present, isLeft = true) && _check(present.right, present, isLeft = false)
        }
      }
    }

    if (root != null)
      _check(root.left, root, isLeft = true) && _check(root.right, root, isLeft = false)
    else
      true
  }

  case class TreeNode(value: Int) {
    var left: TreeNode = _
    var right: TreeNode = _
  }

}
