object SymmetricTree {

  case class TreeNode(_value: Int) {
    var value: Int = _value
    var left: TreeNode = _
    var right: TreeNode = _
  }


  def isSymmetric(root: TreeNode): Boolean = {
    def areChildrenSymmetric(left: TreeNode, right: TreeNode): Boolean = {
      if (left == null && right == null) {
        true
      } else if (left == null || right == null || left.value != right.value) {
        false
      } else {
        areChildrenSymmetric(left.right, right.left) && // i.e ↘ ↙
          areChildrenSymmetric(right.right, left.left) // i.e ↙ ↘
      }
    }

    if (root == null) {
      true
    } else {
      areChildrenSymmetric(root.left, root.right)
    }
  }

  def main(args: Array[String]): Unit = {
    val root = TreeNode(0)
    root.left = TreeNode(1)
    root.right = TreeNode(1)

    root.left.right = TreeNode(2)
    root.left.left = TreeNode(3)

    root.right.right = TreeNode(3)
    root.right.left = TreeNode(2)
    val ans = isSymmetric(root)
    if (ans)
      println("The Tree is symmetric")
    else
      println("The Tree is not symmetric")

  }

}
