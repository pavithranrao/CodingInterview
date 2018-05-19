object BinaryTreeBalanced {
  
  def main(args: Array[String]): Unit = {
    """
      |    3
      |   / \
      |  9  20
      |    /  \
      |   15   7
    """.stripMargin


    val root = TreeNode(3)
    val left = TreeNode(9)
    val right = TreeNode(20)

    right.left = TreeNode(15)
    right.right = TreeNode(7)
    root.left = left
    root.right = right

    assert(isBalanced(root))

    """
      |       1
      |      / \
      |     2   2
      |    / \
      |   3   3
      |  / \
      | 4   4
    """.stripMargin
    val root2 = TreeNode(1)
    val rootLeft = TreeNode(2)
    val rootRight = TreeNode(2)

    val lLeft = TreeNode(3)
    val lRight = TreeNode(3)

    lLeft.left = TreeNode(4)
    lLeft.right = TreeNode(4)

    rootLeft.left = lLeft
    rootLeft.right = lRight

    root2.left = rootLeft
    root2.right = rootRight
    assert(!isBalanced(root2))

  }

  def isBalanced(root: TreeNode): Boolean = {
    def isBalancedHelper(node: TreeNode,
                         isBalanced: Boolean = true,
                         depth: Int = 0): (Boolean, Int) = {
      if (isBalanced) {
        if (node == null) {
          (true, 0)
        } else {
          val (leftBalanced, leftDepth) = isBalancedHelper(node.left)
          val (rightBalanced, rightDepth) = isBalancedHelper(node.right)

          if (leftBalanced && rightBalanced) {
            if (leftDepth - rightDepth > 1 || leftDepth - rightDepth < -1) {
              (false, 0)
            } else {
              (true, (leftDepth max rightDepth) + 1)
            }
          } else {
            (false, 0)
          }
        }
      } else {
        (false, 0)
      }
    }

    isBalancedHelper(root)._1
  }

  case class TreeNode(value: Int) {
    var left: TreeNode = _
    var right: TreeNode = _
  }

}
